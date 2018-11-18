(ns forest-run.core-test
  (:require [clojure.test :refer :all]
            [forest-run.core :as core]))

(deftest hand-strength-test
  (let [hand [{:rank :tre :suit :spade}
              {:rank :tre :suit :coppe}
              {:rank :tre :suit :bastoni}
              {:rank :fante :suit :spade
               :trade {:rank :due :suit :coppe}}]]
    (is (= [{:rank :tre :suit :spade :strength 3}
            {:rank :fante :suit :spade
             :trade {:rank :due, :suit :coppe} :strength 2}]
         (core/hand-with-strengths hand)))))

(deftest moves-test
  (is (= #:moves {:valid {:nw [2 0], :n [1 0], :ne [0 0]}, :invalid {}}
         (core/moves [{:deck     [[{:rank :sette, :suit :bastoni, :revealed false}
                                   {:rank :uno, :suit :bastoni, :revealed false}
                                   {:rank :cinque, :suit :spade, :revealed false}]
                                  [{:rank :cavallo, :suit :bastoni, :revealed false}
                                   {:rank :sette, :suit :denari, :revealed false}
                                   {:rank :due, :suit :bastoni, :revealed false}]
                                  [{:rank :re, :suit :spade, :revealed false}
                                   {:rank :due, :suit :spade, :revealed false}
                                   {:rank :re, :suit :bastoni, :revealed false}]]
                       :hand     core/starting-hand
                       :position [1 -1]
                       :health   10}])))

  (is (= #:moves {:valid {:nw [2 0], :n [1 0], :ne [0 0]}, :invalid {}}
         (core/moves [{:deck     [[{:rank :re, :suit :spade, :revealed false}
                                   {:rank :due, :suit :spade, :revealed false}
                                   {:rank :re, :suit :bastoni, :revealed false}]
                                  [{:rank :cavallo, :suit :bastoni, :revealed false}
                                   {:rank :sette, :suit :denari, :revealed false}
                                   {:rank :due, :suit :bastoni, :revealed false}]
                                  [{:rank :sette, :suit :bastoni, :revealed false}
                                   {:rank :uno, :suit :bastoni, :revealed false}
                                   {:rank :cinque, :suit :spade, :revealed false}]]
                       :hand     core/starting-hand
                       :position [1 -1]
                       :health   10}])))

  (is (= #:moves {:valid {}, :invalid {:nw [2 0], :n [1 0], :ne [0 0]}}
         (core/moves [{:deck     [[{:rank :re, :suit :spade, :revealed false}
                                   {:rank :due, :suit :spade, :revealed false}
                                   {:rank :re, :suit :bastoni, :revealed false}]
                                  [{:rank :cavallo, :suit :bastoni, :revealed false}
                                   {:rank :sette, :suit :denari, :revealed false}
                                   {:rank :due, :suit :bastoni, :revealed false}]
                                  [{:rank :sette, :suit :bastoni, :revealed false}
                                   {:rank :uno, :suit :bastoni, :revealed false}
                                   {:rank :cinque, :suit :spade, :revealed false}]]
                       :hand     core/starting-hand
                       :position [1 -1]
                       :health   1}])))

  (is (= #:moves {:valid {:nw [2 0], :n [1 0]}, :invalid {:ne [0 0]}}
         (core/moves [{:deck     [[{:rank :re, :suit :spade, :revealed false}
                                   {:rank :due, :suit :spade, :revealed false}
                                   {:rank :due, :suit :bastoni, :revealed false}]
                                  [{:rank :cavallo, :suit :bastoni, :revealed false}
                                   {:rank :sette, :suit :denari, :revealed false}
                                   {:rank :due, :suit :bastoni, :revealed false}]
                                  [{:rank :sette, :suit :bastoni, :revealed false}
                                   {:rank :uno, :suit :bastoni, :revealed false}
                                   {:rank :cinque, :suit :spade, :revealed false}]]
                       :hand     core/starting-hand
                       :position [1 -1], :health 1}]))))
