(ns ^:figwheel-always class-pick.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as string]))

(enable-console-print!)

;; initialize app state with test classes so you don't have to enter a bunch manually every time.

(defonce app-state
  (atom {:classes
         [{:crn "11155" :title "Operating Systems"      :days "Monday"  :time_start "13:10" :time_end "14:00" :state :unpicked}
          {:crn "11156" :title "Operating Systems"      :days "Monday"  :time_start "13:10" :time_end "15:00" :state :unpicked}

          {:crn "12072" :title "Mobile App Development" :days "Monday"  :time_start "11:00" :time_end "11:50" :state :unpicked}
          {:crn "12633" :title "Mobile App Development" :days "Monday"  :time_start "15:10" :time_end "16:00" :state :unpicked}

          {:crn "10541" :title "Intro to Literature"    :days "Monday"  :time_start "10:00" :time_end "10:50" :state :unpicked}
          {:crn "10542" :title "Intro to Literature"    :days "Monday"  :time_start "11:00" :time_end "11:50" :state :unpicked}
          {:crn "10543" :title "Intro to Literature"    :days "Tuesday" :time_start "08:00" :time_end "09:15" :state :unpicked}
          {:crn "10544" :title "Intro to Literature"    :days "Tuesday" :time_start "09:25" :time_end "10:40" :state :unpicked}
          {:crn "12178" :title "Intro to Literature"    :days "Monday"  :time_start "11:00" :time_end "11:50" :state :unpicked}
          {:crn "12596" :title "Intro to Literature"    :days "Monday"  :time_start "10:00" :time_end "10:50" :state :unpicked}

          {:crn "11428" :title "Linear Algebra"         :days "Monday"  :time_start "13:10" :time_end "14:00" :state :unpicked}
          {:crn "11739" :title "Linear Algebra"         :days "Monday"  :time_start "09:00" :time_end "09:50" :state :unpicked}

          {:crn "10499" :title "Calculus-Analytic Geometry II" :days "Monday" :time_start "10:00" :time_end "10:50" :state :unpicked}
          {:crn "10501" :title "Calculus-Analytic Geometry II" :days "Monday" :time_start "09:00" :time_end "09:50" :state :unpicked}


          {:crn "1000" :title "Test 1" :days "Monday"    :time_start "8:00" :time_end "8:50" :state :unpicked}
          {:crn "1001" :title "Test 2" :days "Monday"    :time_start "9:00" :time_end "9:50" :state :unpicked}
          {:crn "1002" :title "Test 3" :days "Monday"    :time_start "9:00" :time_end "9:50" :state :unpicked}
          {:crn "1003" :title "Test 4" :days "Tuesday"   :time_start "9:00" :time_end "9:50" :state :unpicked}
          {:crn "1004" :title "Test 5" :days "Wednesday" :time_start "9:00" :time_end "9:50" :state :unpicked}]}))

(println app-state)

(defn class-display-name [{:keys [crn title day]}]
  (str "CRN: " crn " Title: " title " Day: " day))

(defn parse-class [class-str]
  {:crn "9999" :title class-str :days "MTWRF"})


(defn class-list-view [class owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
            (dom/tr nil
                    (dom/td nil (:crn class))
                    (dom/td nil (:title class))
                    (dom/td nil (:days class))
                    (dom/td nil (:time_start class))
                    (dom/td nil (:time_end class))
                    (dom/button #js {:onClick (fn [e] (put! delete @class))}
                                "Delete")))))


(defn class-picker-view [class owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [check]}]
            (dom/tr #js {:className (name (:state class))}
                    (dom/td nil (:crn class))
                    (dom/td nil (:title class))
                    (dom/td nil (:days class))
                    (dom/td nil (:time_start class))
                    (dom/td nil (:time_end class))
                    (dom/input #js {:type "checkbox"
                                    :checked (= (:state class) :picked)
                                    :disabled (= (:state class) :conflicts)
                                    :onClick (fn [e] (put! check @class))})))))

(defn add-class [data owner]
  (let [new-crn (-> (om/get-node owner "new-crn")
                      .-value)
        new-title (-> (om/get-node owner "new-title")
                      .-value)
        new-days (-> (om/get-node owner "new-days")
                     .-value)
        new-start (-> (om/get-node owner "new-start")
                      .-value)
        new-end (-> (om/get-node owner "new-end")
                    .-value)]
    (when (not (or (empty? new-crn)
                   (empty? new-title)
                   (empty? new-days)
                   (empty? new-start)
                   (empty? new-end)))
      (om/transact! data :classes #(conj % {:crn new-crn
                                            :title new-title
                                            :days new-days
                                            :time_start new-start
                                            :time_end new-end
                                            :state :unpicked})))))

(defn classes-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:delete (chan)})
    om/IWillMount
    (will-mount [_]
                (let [delete (om/get-state owner :delete)]
                  (go (loop []
                        (let [class (<! delete)]
                          (om/transact! data :classes
                                        (fn [xs] (vec (remove #(= class %) xs))))
                          (recur))))))
    om/IRenderState
    (render-state [this state]
                  (dom/div nil
                           (dom/h2 nil "Class Editor")
                           (dom/div nil
                                    (dom/table nil
                                           (dom/tr nil
                                                   (dom/th nil "CRN")
                                                   (dom/th nil "Title")
                                                   (dom/th nil "Days")
                                                   (dom/th nil "Start")
                                                   (dom/th nil "End")
                                                   (dom/th nil "Add Class"))
                                            (dom/tr nil
                                                    (dom/td nil (dom/input #js {:type "text" :ref "new-crn"}))
                                                    (dom/td nil (dom/input #js {:type "text" :ref "new-title"}))
                                                    (dom/td nil (dom/input #js {:type "text" :ref "new-days"}))
                                                    (dom/td nil (dom/input #js {:type "text" :ref "new-start"}))
                                                    (dom/td nil (dom/input #js {:type "text" :ref "new-end"}))
                                                    (dom/td nil (dom/button #js {:onClick #(add-class data owner)} "Add class")))))
                           (apply dom/table nil
                                  (dom/thead nil
                                    (dom/tr nil
                                            (dom/th nil "CRN")
                                            (dom/th nil "Title")
                                            (dom/th nil "Days")
                                            (dom/th nil "Start")
                                            (dom/th nil "End")
                                            (dom/th nil "Edit")))
                                  (om/build-all class-list-view (:classes data)
                                                {:init-state state}))
))))

(defn day-conflicts [day1 day2]
  (= day1 day2))

(defn hour-str-to-minutes [str]
  (-> str
      (#(string/split % ":"))
      (#(let [hr (js/parseInt (nth % 0))
             mt (js/parseInt (nth % 1))]
         (+ (* hr 60) mt)))))


(defn hour-conflicts [beg1 end1 beg2 end2]
  (let [s1 (hour-str-to-minutes beg1)
        e1 (hour-str-to-minutes end1)
        s2 (hour-str-to-minutes beg2)
        e2 (hour-str-to-minutes end2)]
    (println "s1 " s1 " e1 " e1 " s2 " s2 " e2 " e2)
    (or (and (>= s1 s2) (<= s1 e2))
             (and (>= s2 s1) (<= s2 e1)))))

(defn check-conflicts [class1 class2]
  (let [day1 (:days class1)
        day2 (:days class2)
        beg1 (:time_start class1)
        beg2 (:time_start class2)
        end1 (:time_end class1)
        end2 (:time_end class2)]
    (and (day-conflicts day1 day2)
         (hour-conflicts beg1 end1 beg2 end2))))

(defn toggle-check [class eq]
  (let [theday (:days eq)]
    (if (= class eq)
      (assoc class :state ({:picked :unpicked :unpicked :picked} (:state class)))
      (if (check-conflicts class eq)
        (assoc class :state ({:conflicts :unpicked :unpicked :conflicts} (:state class)))
        class))))

(defn picker-view [data owner]
  (reify
    om/IInitState
    (init-state [_]
                {:check (chan)})
    om/IWillMount
    (will-mount [_]
                (let [check (om/get-state owner :check)]
                  (go (loop []
                        (let [class (<! check)]
                          (om/transact! data :classes
                                        (fn [m]
                                          (vec (map #(toggle-check % class) m))))
                          (recur))))))
    om/IRenderState
    (render-state [this state]
                  (dom/div nil
                           (dom/h2 nil "Class Picker")
                           (apply dom/table nil
                                  (om/build-all class-picker-view (:classes data)
                                                {:init-state state}))))))

(om/root classes-view app-state
         {:target (. js/document (getElementById "classes"))})

(om/root picker-view app-state
         {:target (. js/document (getElementById "picker"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


