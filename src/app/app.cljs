(ns app.app
  (:require [keechma.next.controllers.router]
            [keechma.next.controllers.dataloader]
            [keechma.next.controllers.subscription]
            [app.controllers.form]
            ["react-dom" :as rdom]))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates,
   :keechma/controllers
   {:form {:keechma.controller/params true}}})