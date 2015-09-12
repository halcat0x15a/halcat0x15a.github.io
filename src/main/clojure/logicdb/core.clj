(ns logicdb.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :refer :all]))

(db-rel language p)
(db-rel jvm p)
(db-rel logic p)
(db-rel library p p')

(def facts
  (db [language 'Java]
      [language 'Prolog]
      [language 'Clojure]
      [language 'Scheme]
      [jvm 'Java]
      [jvm 'Clojure]
      [logic 'Prolog]
      [logic 'core.logic]
      [logic 'miniKanren]
      [library 'Clojure 'core.logic]
      [library 'Scheme 'miniKanren]))
