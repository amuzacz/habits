(ns habits.app
  (:require [clojure.string :as str]
            [goog.date :as gdate]
            [goog.object :as gobject]
            [goog.string :as gstring]
            [shadow.dom :as dom]
            [reagent.dom :as rdom]
            [reagent.core :as r]
            [reagent.impl.component :as rcomp]
            [secretary.core :refer [dispatch!] :refer-macros [defroute]]
            [reagent-mui.material.box :refer [box]]
            [reagent-mui.material.dialog :refer [dialog]]
            [reagent-mui.material.dialog-actions :refer [dialog-actions]]
            [reagent-mui.material.dialog-content :refer [dialog-content]]
            [reagent-mui.material.alert :refer [alert]]
            [reagent-mui.material.stack :refer [stack]]
            [reagent-mui.material.circular-progress :refer [circular-progress]]
            [reagent-mui.material.button :refer [button]]
            [reagent-mui.material.icon-button :refer [icon-button]]
            [reagent-mui.material.text-field :refer [text-field]]
            [reagent-mui.material.menu :refer [menu]]
            [reagent-mui.material.menu-item :refer [menu-item]]
            [reagent-mui.material.list :refer [list]]
            [reagent-mui.material.list-item :refer [list-item]]
            [reagent-mui.material.list-item-icon :refer [list-item-icon]]
            [reagent-mui.material.list-item-text :refer [list-item-text]]
            [reagent-mui.material.list-item-button :refer [list-item-button]]
            [reagent-mui.material.list-item-secondary-action :refer [list-item-secondary-action]]
            [reagent-mui.material.divider :refer [divider]]
            [reagent-mui.material.checkbox :refer [checkbox]]
            [reagent-mui.material.typography :refer [typography]]
            [reagent-mui.x.date-calendar :refer [date-calendar]]
            [reagent-mui.x.pickers-day :refer [pickers-day]]
            [reagent-mui.x.localization-provider :refer [localization-provider]]
            [reagent-mui.cljs-time-adapter :refer [cljs-time-adapter]]
            [reagent-mui.lab.masonry :refer [masonry]]
            [reagent-mui.icons.more-vert :refer [more-vert]]
            [reagent-mui.icons.chevron-left :refer [chevron-left]]
            [reagent-mui.icons.chevron-right :refer [chevron-right]]
            [reagent-mui.icons.delete :refer [delete] :rename {delete delete-icon}]
            [reagent-mui.icons.edit :refer [edit] :rename {edit edit-icon}]
            [reagent-mui.icons.add :refer [add] :rename {add add-icon}]
            [reagent-mui.icons.list :refer [list] :rename {list list-icon}]
            [reagent-mui.icons.arrow-downward :refer [arrow-downward] :rename {arrow-downward arrow-downward-icon}]
            [reagent-mui.icons.arrow-upward :refer [arrow-upward] :rename {arrow-upward arrow-upward-icon}]
            [reagent-mui.icons.file-download :refer [file-download] :rename {file-download file-download-icon}]
            [reagent-mui.icons.file-upload :refer [file-upload] :rename {file-upload file-upload-icon}]
            [reagent-mui.icons.calendar-month :refer [calendar-month] :rename {calendar-month calendar-month-icon}]
            [reagent-mui.icons.calendar-today :refer [calendar-today] :rename {calendar-today calendar-today-icon}]
            ["@mui/x-date-pickers/PickersDay" :as MuiPickersDay]
            ["react-vis" :refer [XYPlot FlexibleWidthXYPlot XAxis YAxis VerticalGridLines
                                 HorizontalGridLines LineMarkSeries
                                 AreaSeries LineSeries]]
            ["react-color" :refer [CirclePicker]]
            ["tinycolor2" :as tcolor]
            ["file-saver" :as file-saver]))

;;; State

(def storage (r/atom {:habits []      ; [{:id 1 :name "chess" :color "#8bc34a"}]
                      :timeline {}})) ; {"2023-04-27" {:habits [{:id 1 :value 4}]}}

(def app-state (r/atom {:checked-habits #{}
                        :selected-month (js/Date.)}))

;;; Navigation

(def routes (r/atom []))

(defn open-route
 ([route]
  (open-route route false))
 ([route is-root]
  (if is-root
    (reset! routes [route])
    (swap! routes #(conj % route)))
  (dispatch! route)))

(defn top-route [route]
  (open-route route true))

(defn pop-route []
  (if-let [route (-> @routes pop peek)]
    (do (swap! routes pop)
        (dispatch! route))
    (dispatch! "/")))

;;; Utils

(def log (.-log js/console))

(defn to-json [data]
  (js/JSON.stringify (clj->js data)))

(defn from-json [json]
  (js->clj (js/JSON.parse json) {:keywordize-keys true}))

(defn in? [coll val]
  (some? (some #(= val %) coll)))

(defn find-record [coll key val]
  (some #(when (= (get % key) val) %) coll))

(defn pad-with-zeros [v l]
  (str (str/join "" (take (- l (count v)) (repeat "0"))) v))

(defn format-date [date]
  (str/join "-" [(str (.getFullYear date))
                 (pad-with-zeros (str (inc (.getMonth date))) 2)
                 (pad-with-zeros (str (.getDate date)) 2)]))

(defn next-date [date]
  (let [base (js/Date. date)]
    (.setDate base (inc (.getDate base)))
    base))

(defn prev-date [date]
  (let [base (js/Date. date)]
    (.setDate base (dec (.getDate base)))
    base))

(defn is-today [date]
  (= (format-date date) (format-date (js/Date.))))

(defn str-to-time [date]
  (.getTime (js/Date. date)))

(defn first-day-of-month [date]
  ; Use UTC date to obtain valid time from this date
  (js/Date. (js/Date.UTC (.getFullYear date) (.getMonth date) 1)))

(defn last-day-of-month [date]
  (js/Date. (js/Date.UTC (.getFullYear date) (inc (.getMonth date)) 0)))

(defn mix-colors [colors]
  (reduce #(.toHexString (tcolor/mix %1 %2)) colors))

(defn move-item [coll item pos]
  (let [rm-item (partial filter #(not= % item))
       [left right] (split-at pos coll)]
    (into [] (concat (rm-item left) [item] (rm-item right)))))

(defn index-of [coll item]
  (reduce-kv #(if (= %3 item) (reduced %2) %1) nil coll))

(defn next-habit-id []
  (inc (apply max (conj (map :id (:habits @storage)) 0))))

(defn update-habit [id name color]
  (swap! storage (fn [s]
                   (assoc s :habits (-> (filter #(not= (:id %) id) (:habits s))
                                        (conj {:id id :name name :color color})
                                        ((partial into [])))))))

(defn get-habits-at [date]
  (let [data @storage]
    (map #(assoc % :habit (find-record (:habits data) :id (:id %)))
         (:habits (get-in data [:timeline (format-date date)])))))

(defn add-habit-at [date habit-id value]
  (let [date-key (format-date date)
        new-habits (->> (:habits (get-in @storage [:timeline date-key]))
                        (filter #(not= (:id %) habit-id))
                        (into [{:id habit-id :value value}]))]
    (swap! storage #(assoc-in % [:timeline date-key :habits] new-habits))))

(defn remove-habit [habit-id]
  (swap!
   storage
   (fn [storage]
     (let [rm-habit (fn [col] (filter #(not= (:id %) habit-id) col))
           new-habits (rm-habit (:habits storage))
           new-timline (reduce
                        (fn [acc [date info]]
                          (assoc acc date (assoc info :habits (rm-habit (:habits info)))))
                        {}
                        (:timeline storage))]
       (assoc storage :habits new-habits :timeline new-timline)))))

(defn remove-habit-at [date habit-id]
  (let [date-key (format-date date)
        new-habits (->> (:habits (get-in @storage [:timeline date-key]))
                        (filter #(not= (:id %) habit-id)))]
    (swap! storage #(assoc-in % [:timeline date-key :habits] new-habits))))

(defn cut-timeline [timeline start-time end-time]
  (into (sorted-map) (filter #(<= start-time (str-to-time (first %)) end-time) timeline)))

(defn filter-timeline [timeline habit-id]
  (reduce (fn [acc [key entry]]
            (if-let [habits (seq (filter #(= (:id %) habit-id) (:habits entry)))]
              (assoc acc key (assoc entry :habits habits))
              acc))
          {}
          timeline))

(defn validation
 "Pass scheme as {:field-name {validator-fn error-message}}
  Pass fields as {:field-name [value message-atom]}"
 ([scheme]
  (partial validation scheme))
 ([scheme fields & {:keys [separator] :or {separator "\n"}}]
   (doseq [[_ msg] (vals fields)]
     (reset! msg nil))
   (let [valid? #(every? true? (doall (map %1 %2)))]
     (valid? (fn [[field [val error]]]
               (valid? (fn [[is-valid msg]]
                         (if (is-valid val)
                           true
                           (let [e @error]
                             (if (str/blank? e)
                               (reset! error msg)
                               (reset! error (str e separator msg))))))
                       (get scheme field)))
             fields))))

;;; File storage

(defn cast-file-error [error]
  (js/Error. (get {1 "NOT_FOUND_ERR"
                   2 "SECURITY_ERR"
                   3 "ABORT_ERR"
                   4 "NOT_READABLE_ERR"
                   5 "ENCODING_ERR"
                   6 "NO_MODIFICATION_ALLOWED_ERR"
                   7 "INVALID_STATE_ERR"
                   8 "SYNTAX_ERR"
                   9 "INVALID_MODIFICATION_ERR"
                   10 "QUOTA_EXCEEDED_ERR"
                   11 "TYPE_MISMATCH_ERR"
                   12 "PATH_EXISTS_ERR"}
                  (.-code error) (.toString error))))

(defn save-file [dirname filename content]
  (js/Promise.
   (fn [resolve reject]
     (js/window.resolveLocalFileSystemURL
      dirname
      (fn [dir-entry]
        (.getFile
         dir-entry
         filename
         (clj->js {:create true})
         (fn [file-entry]
           (.createWriter
            file-entry
            (fn [writer]
              (set! (.-onwriteend writer) (fn [_] (resolve (count content))))
              (set! (.-onerror writer) #(reject (cast-file-error %)))
              (.write writer content))
            (fn [error] (reject (cast-file-error error)))))
         (fn [error] (reject (cast-file-error error)))))
      (fn [error] (reject (cast-file-error error)))))))

(defn read-file [dirname filename]
  (js/Promise.
   (fn [resolve reject]
     (js/window.resolveLocalFileSystemURL
      dirname
      (fn [dir-entry]
        (.getFile
         dir-entry
         filename
         (clj->js {:create false})
         (fn [file-entry]
           (.file
            file-entry
            (fn [file]
              (let [reader (js/FileReader.)]
                (set! (.-onload reader) #(resolve (.. % -target -result)))
                (set! (.-onerror reader) #(reject (cast-file-error %)))
                (.readAsText reader file)))
            (fn [error] (reject (cast-file-error error)))))
         (fn [error] (reject (cast-file-error error)))))
      (fn [error] (reject (cast-file-error error)))))))

(defn upload-file [file]
  (js/Promise.
   (fn [resolve reject]
     (let [reader (js/FileReader.)]
       (set! (.-onload reader) #(resolve (.. % -target -result)))
       (set! (.-onerror reader) #(reject %))
       (.readAsText reader file)))))


(defn load-storage-from-json [json]
  (let [error (r/atom "")
        cast-types #(assoc % :timeline (update-keys (:timeline %) (fn [k] (key->js k)) ))
        validate (validation {:habits {#(some? %) "No habits entry"}
                              :timeline {#(some? %) "No timeline entry"}})
        is-valid #(if (not (validate {:habits [(:habits %) error]
                                      :timeline [(:timeline %) error]}))
                    (throw (js/Error. @error))
                    %)]
    (some-> (from-json json)
            (cast-types)
            (is-valid)
            ((partial reset! storage)))))

(defn load-storage []
  (-> (if (= (.-platformId js/cordova) "browser")
        (js/Promise. (fn [resolve _]
                       (resolve (js/localStorage.getItem "storage"))))
        (-> (read-file (.. js/cordova -file -dataDirectory) "storage.json")
            (.catch #(if (= (.-message %) "NOT_FOUND_ERR") nil (throw %)))))
      (.then load-storage-from-json)))

(defn dump-storage [_ _ _ state]
  (let [json (to-json state)]
    (if (= (.-platformId js/cordova) "browser")
      (js/localStorage.setItem "storage" json)
      (save-file (.. js/cordova -file -dataDirectory) "storage.json" json))))

(defn load-app-state []
  (let [cast-types #(assoc % :checked-habits (set (:checked-habits %))
                            :selected-month (js/Date. (:selected-month %)))]
    (some-> (js/localStorage.getItem "app-state")
            (from-json)
            (cast-types)
            ((partial reset! app-state)))))

(defn dump-app-state [_ _ _ state]
  (js/localStorage.setItem "app-state" (to-json state)))

;;; Layout

(def app (js/document.getElementById "app"))
(def popups (js/document.getElementById "popups"))

(defn show-error-popup [props]
  (js/Promise.
   (fn [resolve reject]
     (rdom/render [dialog
                    {:open true}
                    [dialog-content
                      (into [stack] (for [m (:messages props)]
                                      [alert {:severity "error"
                                              :style {:white-space "pre-wrap"}}
                                             m]))]
                    [dialog-actions
                      [button {:on-click #(do (rdom/unmount-component-at-node popups)
                                            (resolve true))}
                              "OK"]]]
                  popups))))

(defn show-confirm-popup [props]
  (js/Promise.
   (fn [resolve reject]
     (rdom/render [dialog
                  {:open true}
                  [dialog-content (:message props)]
                  [dialog-actions
                    [button {:on-click #(rdom/unmount-component-at-node popups)}
                           "No"]
                    [button {:on-click #(do (rdom/unmount-component-at-node popups)
                                            (resolve true))}
                            "Yes"]]]
                  popups))))

(defn layout [node]
  [box {:display "flex" :height "100%"}
    [box {:m "auto" :min-width "400px" :height "100%"} node]])

(defn panel [header & nodes]
  (into
    [box {:display "flex" :flex-direction "column" :height "100%" :p "0px 10px"}]
    [header (into [box {:display "flex" :flex-direction "column" :height "100%"
                        :p "10px 0px"}]
                  nodes)]))

(defn preloader []
  [box {:display "flex" :height "100%" :align-items "center" :justify-content "center"}
       [circular-progress]])

(defn header [& {:keys [left text right]}]
  [box {:display "flex" :flex-direction "row" :justify-content "space-between" :p "5px 0px"}
    (or left [:span {:style {:width 40}} ""])
    [box  {:component "h1" :flex 1 :m 0 :p "5px 0px 0px 0px"
           :text-align "center" :font-size "24px" :font-weight "normal"}
         (or text "")]
    (or right [:span {:style {:width 40}} ""])])

;;; Habits list

(defn habits-menu []
  (let [anchor (r/atom js/undefined)
        opened (r/atom false)
        handle-export (fn [event]
                        (file-saver/saveAs (js/Blob. [(to-json @storage)]
                                                     {:type "text/json;charset=utf-8"})
                                           "habits.json"))
        handle-import (fn [event]
                        (-> (upload-file (first (.. event -target -files)))
                            (.then #(load-storage-from-json %))
                            (.catch #(show-error-popup {:messages [(.-message %)]}))))]
    (fn []
      [:div
        [icon-button {:on-click #(do
                                   (reset! anchor (. % -currentTarget))
                                   (reset! opened true))}
                     [more-vert]]
        [menu {:anchor-el @anchor
               :open @opened
               :on-close #(reset! opened false)}
               [menu-item {:on-click #(open-route "/habit/add")}
                          [list-item-icon [add-icon]] [list-item-text "Add"]]
               [menu-item {:on-click #(open-route "/habits")}
                          [list-item-icon [list-icon]] [list-item-text "List"]]
               [menu-item {:on-click handle-export}
                          [list-item-icon [file-download-icon]] [list-item-text "Export"]]
               [menu-item [list-item-icon [file-upload-icon]]
                          [list-item-button {:variant "text"
                                             :component "label"
                                             :style {:padding "0px" :background-color "transparent"}}
                                            "Import"
                                            [:input {:hidden true
                                                     :accpet "text/*"
                                                     :type "file"
                                                     :on-change handle-import}]]]]])))
(defn no-habits []
  [box {:display "flex" :height "100%" :align-items "center" :justify-content "center"}
    [:span {:style {:color "#2196f3"}} "No habits, please add some."]])

(defn habits-list []
  (let [habits (r/cursor storage [:habits])
        move-up (fn [habit]
                  (swap! habits #(move-item % habit (dec (index-of % habit)))))
        move-down (fn [habit]
                    (swap! habits #(move-item % habit (+ (index-of % habit) 2))))]
    (fn []
      (let [all-habits @habits]
        (if (not (seq all-habits))
          [no-habits]
          [list (for [i all-habits] ^{:key i}
                  [list-item
                    {:sx {:p "8px 0px"}}
                    [:div {:style {:display "flex" :flex-direction "row" :width "100%"}}
                      [:div {:style {:flex 1 :display "flex" :align-items "center"}} (:name i)]]
                    [list-item-secondary-action
                      [icon-button {:disabled (= i (first all-habits))
                                    :on-click #(move-up i)}
                                   [arrow-upward-icon]]
                      [icon-button {:disabled (= i (last all-habits))
                                    :on-click #(move-down i)}
                                   [arrow-downward-icon]]
                      [icon-button {:style {:color (:color i)}
                                    :on-click #(open-route (str "/habit/edit/" (:id i)))}
                                   [edit-icon]]]])])))))

(defn habits-panel [props]
  [panel
    [header {:left [icon-button {:on-click #(pop-route)} [chevron-left]]
             :text "Habits"
             :right [habits-menu]}]
    [habits-list]])

;;; Habit

(defn habit-form [props]
  (let [state (r/atom {:habit {:name (:name props) :color (:color props)}
                       :errors {:name nil}})
        name (r/cursor state [:habit :name])
        name-error (r/cursor state [:errors :name])
        color (r/cursor state [:habit :color])
        set-name #(do (reset! name-error nil)
                      (reset! name (.. % -target -value)))
        pick-color #(reset! color (.-hex %))
        delete (fn []
                 (-> (show-confirm-popup {:message "Are you sure you want to delete this habit ?"})
                     (.then #(do (remove-habit (:id props))
                                 (open-route "/habits")))))
        validate (validation {:name {#(not (str/blank? %1)) "This field is required"}})
        submit (fn []
                 (when (validate {:name [@name name-error]})
                   ((:on-submit props) (:habit @state))))]
    (fn []
      [panel
        [header {:left [icon-button {:on-click #(pop-route)} [chevron-left]]
                 :text (:title props)
                 :right [icon-button {:disabled (nil? (:id props))
                                      :on-click delete}
                                     [delete-icon]]}]
        [box {:width "100%"}
          [text-field {:variant "outlined" :label "Name" :full-width true :size "small"
                       :error (some? @name-error)
                       :helper-text @name-error
                       :value @name
                       :on-change #(set-name %)}]]
        [box {:m "20px auto"}
             [:> CirclePicker {:color @color
                               :circle-size 35
                               :width 300
                               :on-change #(pick-color %1)}]]
        [box {:mt "auto"} [button {:variant "contained" :full-width true
                                   :on-click #(submit)} "Save"]]])))

(defn habit-add-form []
  [habit-form {:title "Add habit"
               :name ""
               :color "#8bc34a"
               :on-submit (fn [fields]
                            (update-habit (next-habit-id) (:name fields) (:color fields))
                            (open-route "/habits"))}])

(defn habit-edit-form [props]
  (if-let [habit (find-record (:habits @storage) :id (:id props))]
    [habit-form {:id (:id habit)
                 :title (:name habit)
                 :name (:name habit)
                 :color (:color habit)
                 :on-submit (fn [fields]
                              (update-habit (:id props) (:name fields) (:color fields))
                              (open-route "/habits"))}]))

;;; Day

(defn daily-list [props]
  (let [date (:date props)
        checked-habits (get-habits-at date)
        on-check #(if (.. %1 -target -checked)
                    (add-habit-at date %2 nil)
                    (remove-habit-at date %2))
        on-fill #(add-habit-at date %2 (float (.. %1 -target -value)))]
    [list (for [i (:habits @storage)] ^{:key i}
            [list-item
              {:sx {:p "8px 0px"}}
              [:div {:style {:display "flex" :flex-direction "row" :width "100%"}}
                [checkbox {:style {:color (:color i)}
                           :checked (in? (map :id checked-habits) (:id i))
                           :on-change #(on-check % (:id i))}]
                [:div {:style {:flex 1 :display "flex" :align-items "center"}} (:name i)]
                [text-field {:style {:self-align "end" :margin-right "9px"}
                             :sx {:width "25%" :text-align "right"}
                             :size "small"
                             :placeholder "minutes"
                             :label "time"
                             :type "number"
                             :value (:value (find-record checked-habits :id (:id i)))
                             :on-change #(on-fill % (:id i))}]]])]))

(defn day-panel [props]
  [panel
    [header {:text (if (is-today (:date props))
                     "Today"
                     (.toLocaleDateString (:date props) "en-US" #js {:weekday "long" :day "numeric" :month "numeric"}))
             :right [:div {:style {:display "flex" :flex-direction "row"}}
                      [icon-button {:on-click #(open-route (str "/calendar/" (format-date (:date props))))}
                        [calendar-month-icon]]
                      [habits-menu]]}]
    (if (not (seq (:habits @storage)))
      [no-habits]
      [daily-list {:date (:date props)}])
    [:div {:style {:display "flex" :flex-direction "row" :justify-content "center" :height "100%"}}
      [:div {:style {:display "flex" :align-self "flex-end"}}
        [icon-button {:size "large"
                      :on-click #(top-route (str "/day/" (-> props :date prev-date format-date)))}
          [chevron-left]]
        [icon-button {:size "large"
                      :on-click #(top-route (str "/day/" (-> props :date next-date format-date)))}
          [chevron-right]]]]])

;;; Calendar

(defn calendar-day [^js props checked-habits]
  (r/create-element MuiPickersDay/PickersDay
                   (if-let [habits (seq (filter (fn [t] (some #{(:id t)} checked-habits))
                                                (get-habits-at (. props -day))))]
                     (-> (js->clj props)
                         (assoc :selected true
                                :style {:backgroundColor (mix-colors (map #(get-in % [:habit :color]) habits))})
                         (clj->js))
                     props)))

(defn calendar-habits [props]
  (into [masonry {:columns 3 :spacing 1}]
        (for [i (:habits @storage)] ^{:key i}
          [:div {:style {:display "flex" :flex-direction "row"}}
                [checkbox {:style {:color (:color i)}
                           :value (:id i)
                           :checked (in? (:checked-habits props) (:id i))
                           :on-change (:on-change props)}]
                [:div {:style {:flex 1 :display "flex" :align-items "center"}} (:name i)]])))

(defn habit-series [checked-habits start-date end-date]
  (map-indexed
   (fn [idx habit]
     [:> AreaSeries {:className (str "area-elevated-series-" (inc idx))
                     :color (:color habit)
                     :opacity 0.5
                     :data (for [[date entry] (-> (:timeline @storage)
                                                  (cut-timeline (.getTime start-date) (.getTime end-date))
                                                  (filter-timeline (:id habit)))
                                 :let [value (-> entry :habits first :value)]
                                 :when (pos? value)]
                             {:x0 (- (str-to-time date) 86400000)
                              :x (str-to-time date)
                              :y value
                              :y0 0})}])
   (filter (fn [t] (some #{(:id t)} checked-habits)) (:habits @storage))))

(defn calendar-panel [props]
  (let [checked-habits (r/cursor app-state [:checked-habits])
        selected-month (r/cursor app-state [:selected-month])]
    (fn []
      (let [start-date (first-day-of-month @selected-month)
            end-date (last-day-of-month @selected-month)
            check-habit (fn [e checked-habits]
                          (swap! checked-habits
                                 #(if (.. e -target -checked)
                                    (conj % (int (.. e -target -value)))
                                    (disj % (int (.. e -target -value))))))]
        [panel
          [header {:left [icon-button {:on-click #(top-route (str "/day/" (format-date (:date props))))}
                           [chevron-left]]
                   :text "Calendar"
                   :right [icon-button {:on-click #(top-route "/")} [calendar-today-icon]]}]
          [localization-provider {:date-adapter cljs-time-adapter}
                                 [date-calendar {:on-change #(top-route (str "/day/" (format-date %)))
                                                 :on-month-change #(reset! selected-month (.-date %))
                                                 :default-calendar-month (gdate/DateTime. @selected-month)
                                                 :slots {:day #(calendar-day %1 @checked-habits)}}]]
          [box
            (when-let [series (seq (habit-series @checked-habits start-date end-date))]
              (into [:> FlexibleWidthXYPlot {:height 200
                                             :x-type "time"
                                             :x-domain [(str-to-time start-date)
                                                        (str-to-time end-date)]}]
                (concat [[:> VerticalGridLines]
                        [:> HorizontalGridLines]
                        [:> XAxis {:style {:text {:fill "#000000"}}
                                   :tick-format #(.getDate %)
                                   }]
                        [:> YAxis {:title "minutes"
                                   :style {:text {:fill "#000000"}}}]]
                        series)))]
          [box {:mt "auto"}
               [calendar-habits {:checked-habits @checked-habits
                                 :on-change #(check-habit % checked-habits)}]]]))))
;;; Routes

(defn do-layout [node]
  (rdom/render [layout node] app))

(defn on-ready [_]
  (do-layout [preloader])
  (-> (load-storage)
      (.then #(do
                (load-app-state)
                (add-watch storage :dump dump-storage)
                (add-watch app-state :dump dump-app-state)
                (top-route "/")))
      (.catch #(show-error-popup {:messages [(.-message %)]}))))

(js/document.addEventListener "deviceready" on-ready false)

(defn ^:dev/after-load start []
  (log "Starting...")
  ;(dispatch! "/")
  )

(defroute "/" {:as params}
  (do-layout [day-panel {:date (js/Date.)}]))

(defroute "/day/:date" [date]
  (do-layout [day-panel {:date (js/Date. date)}]))

(defroute "/habits" {:as params}
  (do-layout [habits-panel]))

(defroute "/habit/add" {:as params}
  (do-layout [habit-add-form]))

(defroute "/habit/edit/:id" [id]
  (do-layout [habit-edit-form {:id (int id)}]))

(defroute "/calendar/:date" [date]
  (do-layout [calendar-panel {:date (js/Date. date)}]))

(defn init []
  (start))