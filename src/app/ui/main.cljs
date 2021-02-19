(ns app.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub use-meta-sub dispatch]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.hooks :as hooks]
            [helix.core :as hx :refer [$]]
            [helix.dom :as d]
            [app.controllers.form :as f]
            ["react-color" :refer [SketchPicker]]))

(defnc
  Errors
  [{:keys [errors] :as props}]
  (when errors
    (d/ul {:class "error-messages"}
      (map-indexed
        (fn [i e]
          (d/li
            {:key i
             :class "text-sm text-red-800"}
            e))
        errors))))

(defnc ColorInput [{:keys [attr label] :as props}]
  (let [id (str attr)
        value-getter (hooks/use-callback [id] #(f/get-data-in % attr))
        errors-getter (hooks/use-callback [id] #(f/get-errors-in % attr))
        value (use-meta-sub props :form value-getter)
        errors (use-meta-sub props :form errors-getter)
        [is-picker-open set-is-picker-open] (hooks/use-state false)]
    (d/div
      {:class "mb-4"}
      (d/label {:for id :class "block"} label)
      (d/div
        {:class "w-32 h-8 border border-black"
         :style (when value {:background value})
         :onClick #(set-is-picker-open not)})
      (d/button
        {:class "bg-gray-200 border border-gray-300 text-gray-600 rounded py-1 px-2 cursor-pointer text-xs mt-1"
         :type "button"
         :onClick #(dispatch props :form :reset-color)}
        "Remove Color")
      (when is-picker-open
        (d/div
          {:class "absolute z-2"}
          (d/div
            {:class "fixed inset-0"
             :onClick #(set-is-picker-open not)})
          ($ SketchPicker
            {:color (str value)
             :onChange
             #(dispatch props :form :keechma.form.on/change {:value (.-hex %) :attr attr :validate-immediately? true})})))
      ($ Errors {:errors errors}))))

(defnc TextInput [{input-type :type
                   :keys [attr label placeholder]
                   :as props}]
  (let [id (str attr)
        value-getter (hooks/use-callback [id] #(f/get-data-in % attr))
        errors-getter (hooks/use-callback [id] #(f/get-errors-in % attr))
        value (use-meta-sub props :form value-getter)
        errors (use-meta-sub props :form errors-getter)]
    (d/div
      {:class "mb-4"}
      (d/label {:for id :class "block"} (or label placeholder))
      (d/input
        {:value (str value)
         :type (or input-type "text")
         :placeholder (or placeholder label)
         :class ["border p-1" (if (seq errors) "border-red-800" "border-gray-300")]
         :onChange #(dispatch props :form :keechma.form.on/change {:value (.. % -target -value) :attr attr})
         :onBlur #(dispatch props :form :keechma.form.on/blur {:value (.. % -target -value) :attr attr})})
      ($ Errors {:errors errors}))))

(def form-state-getter :app.controllers.form/form)

(defnc MainRenderer [props]
  (let [form-state (use-meta-sub props :form form-state-getter)
        form-state-type (get-in form-state [:state :type])]
    (cljs.pprint/pprint form-state)
    (d/form
      {:class "m-4"
       :onSubmit (fn [e]
                   (.preventDefault e)
                   (dispatch props :form :keechma.form.on/submit))}
      (when (= :error form-state-type)
        (d/div
          {:class "bg-red-200 text-red-900 px-4 py-2 mb-3"}
          (ex-message (get-in form-state [:state :cause]))))
      ($ TextInput
        {:attr :username
         :label "Username"
         & props})
      ($ TextInput
        {:attr :password
         :label "Password"
         & props})
      ($ TextInput
        {:attr :password-confirmation
         :label "Password Confirmation"
         & props})
      ($ ColorInput
        {:attr :favorite-color
         :label "Favorite Color"
         & props})
      (d/hr {:class "border border-gray-500 my-3"})
      (str form-state-type)
      (d/br)
      (d/button
        {:class "bg-gray-200 border border-gray-300 text-gray-600 rounded py-2 px-4 cursor-pointer mr-2"
         :type "button"
         :onClick #(dispatch props :form :keechma.form.on/reset)}
        "Reset")
      (d/button {:class "bg-blue-400 text-white rounded py-2 px-4 cursor-pointer"} "Submit"))))

(def Main (with-keechma MainRenderer))