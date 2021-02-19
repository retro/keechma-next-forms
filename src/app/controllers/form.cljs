(ns app.controllers.form
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.pipelines.core :as pp :refer-macros [pipeline!]]
            [clojure.set :as set]
            [promesa.core :as p]))

(derive :form ::pipelines/controller)

(defn as-path [path]
  (if (sequential? path) path [path]))

(defn calculate-dirty-fields [prev current]
  (let [prev-keys (-> prev keys set)
        current-keys (-> current keys set)
        all-keys (set/union prev-keys current-keys)]
    (reduce
      (fn [acc k]
        (if (not= (get prev k) (get current k))
          (conj acc (as-path k))
          acc))
      #{}
      all-keys)))

(defn get-errors [meta-state]
  (get-in meta-state [::form :errors]))

(defn get-data [meta-state]
  (get-in meta-state [::form :data]))

(defn get-initial-data [meta-state]
  (get-in meta-state [::form :initial-data]))

(defn get-data-in [meta-state path]
  (get-in (get-data meta-state) (as-path path)))

(defn get-errors-in [meta-state path]
  (let [form-state (::form meta-state)
        path' (as-path path)
        is-dirty (or (contains? (:cached-dirty-paths form-state) path')
                   (contains? (:dirty-paths form-state) path'))]
    (when is-dirty
      (get (:errors form-state) path'))))

(defn path-valid? [meta-state path]
  (empty? (get-errors-in meta-state path)))

(def path-invalid? (complement path-valid?))

(defn valid? [meta-state]
  (->> (get-in meta-state [::form :errors])
    vals
    (filter seq)
    empty?))

(def invalid? (complement valid?))

(defn submit-attempted? [meta-state]
  (get-in meta-state [::form :submit-attempted?]))

(defn mark-dirty-and-validate
  ([meta-state validator] (mark-dirty-and-validate meta-state validator true))
  ([meta-state validator only-dirty]
   (let [data (get-data meta-state)
         initial-data (get-initial-data meta-state)]
     (if only-dirty
       (let [errors (when validator (validator data))
             dirty-paths (calculate-dirty-fields initial-data data)]
         (update meta-state ::form merge {:errors errors :dirty-paths (set dirty-paths)}))
       (let [errors (when validator (validator data))
             dirty-paths (set (keys errors))
             cached-dirty-paths (set (get-in meta-state [::form :cached-dirty-paths]))]

         (update meta-state ::form merge {:errors             errors
                                          :dirty-paths        dirty-paths
                                          :cached-dirty-paths (set/union dirty-paths cached-dirty-paths)}))))))

(defn assoc-data-in [meta-state path value]
  (let [path' (as-path path)]
    (assoc-in meta-state (concat [::form :data] path') value)))

(defn assoc-data [meta-state value]
  (assoc-in meta-state [::form :data] value))

(defn handle-on-change [meta-state validator payload]
  (let [path (as-path (:attr payload))
        should-validate-immediately? (or (:validate-immediately? payload) (path-invalid? meta-state path))]
    (cond-> meta-state
      true (assoc-data-in (:attr payload) (:value payload))
      should-validate-immediately? (mark-dirty-and-validate validator))))

(defn handle-on-blur [meta-state validator]
  (mark-dirty-and-validate meta-state validator))

(defn handle-on-submit [meta-state validator]
  (-> meta-state
    (mark-dirty-and-validate validator false)
    (assoc-in [::form :submit-attempted?] true)))

(defn initialize-form [meta-state]
  (assoc meta-state ::form {:state {:type :mounting}}))

(defn mount-form [meta-state value]
  (-> meta-state
    (update ::form merge {:submit-attempted?  false
                          :dirty-paths        #{}
                          :cached-dirty-paths #{}
                          :data               value
                          :initial-data       value
                          :state              {:type :mounted}})))

(defn handle-error [meta-state error]
  (-> meta-state
    (assoc-in [::form :state] {:type :error :cause error})))

(defn get-form-data [meta-state]
  (get meta-state ::form))

(defn mark-as-submitting [meta-state]
  (assoc-in meta-state [::form :state] {:type :submitting}))

(defn mark-as-submitted [meta-state]
  (assoc-in meta-state [::form :state] {:type :submitted}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn username-validator [value attr data]
  (cond-> []
    (nil? value) (conj "Username can't be empty")
    (> 5 (count (str value))) (conj "Username must be at least 5 characters long")
    (contains? #{"admin" "editor"} value) (conj (str value " is a reserved word"))))

(defn password-validator [value attr data]
  (cond-> []
    (nil? value) (conj "Password can't be empty")
    (> 8 (count (str value))) (conj "Password must be at least 8 characters long")))

(defn password-confirmation-validator [value attr data]
  (let [password-value (get data :password)]
    (cond-> []
      (not= value password-value) (conj "Password and password confirmation must match"))))

(defn favorite-color-validator [value _ _]
  (cond-> []
    (empty? value) (conj "Please select favorite color")
    (= "#000000" value) (conj "Black is not a color")))

(defn validate [validators data]
  (reduce-kv
    (fn [acc attr validator]
      (let [errors (validator (get data attr) attr data)]
        (assoc acc (as-path attr) errors)))
    {}
    validators))

(defn load-initial-data []
  (p/create
    (fn [resolve _]
      (js/setTimeout #(resolve {}) 100))))

(defn submit-data [data]
  (p/create
    (fn [resolve reject]
      (js/setTimeout
        #(if (= "retro" (:username data))
           (reject (ex-info "Username is taken" {}))
           (resolve data))
        1000))))

(def pipelines
  {:keechma.on/start
   (pipeline! [value {:keys [meta-state*]}]
     (pp/swap! meta-state* initialize-form)
     (load-initial-data)
     (pp/swap! meta-state* mount-form value))
   :keechma.form.on/change (pipeline! [value {:keys [validator meta-state*]}]
                             (pp/swap! meta-state* handle-on-change validator value))
   :keechma.form.on/blur (pipeline! [value {:keys [validator meta-state*]}]
                           (pp/swap! meta-state* handle-on-blur validator value))
   :keechma.form.on/submit (-> (pipeline! [value {:keys [validator meta-state*]}]
                                 (pp/swap! meta-state* handle-on-submit validator)
                                 (when (valid? @meta-state*)
                                   (pipeline! [value {:keys [meta-state*]}]
                                     (pp/swap! meta-state* mark-as-submitting)
                                     (submit-data (get-data @meta-state*))
                                     (pp/swap! meta-state* mark-as-submitted)
                                     (p/delay 1000)
                                     (pp/swap! meta-state* mount-form {})))
                                 (rescue! [error]
                                   (pp/swap! meta-state* handle-error error)))
                             pp/use-existing
                             pp/dropping)
   :keechma.form.on/reset (pipeline! [value {:keys [validator meta-state*]}]
                            (pp/swap! meta-state* (fn [meta-state]
                                                    (let [initial-data (get-initial-data meta-state)]
                                                      (-> meta-state
                                                        (assoc-data initial-data)
                                                        (mark-dirty-and-validate validator))))))
   :reset-color (pipeline! [value {:keys [meta-state* validator]}]
                  (pp/swap! meta-state* handle-on-change validator {:attr :favorite-color
                                                                    :value nil
                                                                    :validate-immediately? true}))})

(defmethod ctrl/prep :form [ctrl]
  (-> ctrl
    (pipelines/register pipelines)
    (assoc :validator (partial validate {:username username-validator
                                         :password password-validator
                                         :password-confirmation password-confirmation-validator
                                         :favorite-color favorite-color-validator}))))


(comment
  (def data {:username "admin1"
             :password "1234567890"
             :password-confirmation "1234567890"})
  (def validators {:username username-validator
                   :password password-validator
                   :password-confirmation password-confirmation-validator
                   :favorite-color favorite-color-validator})

  (validate validators data)
  (calculate-dirty-fields {} data)
  )