(ns cljs.closure-tests
  (:require [cljs.analyzer :as ana]
            [cljs.env :as env])
  (:refer-clojure :exclude [compile])
  (:use cljs.closure)
  (:use clojure.test))

(deftest test-make-preamble
  (testing "no options"
    (is (= "" (make-preamble {}))))
  (testing "nodejs"
    (testing "with default hashbang"
      (is (= "#!/usr/bin/env node\n" (make-preamble {:target :nodejs}))))
    (testing "with custom hashbang"
      (is (= "#!/bin/env node\n" (make-preamble {:target :nodejs
                                                 :hashbang "/bin/env node"}))))
    (testing "with no hashbang"
      (is (= "" (make-preamble {:target :nodejs
                                :hashbang false})))
      (testing "and preamble"
        (is (= "var preamble1 = require(\"preamble1\");\n"
              (make-preamble {:target :nodejs
                              :hashbang false
                              :preamble ["cljs/preamble1.js"]})))))
    (testing "with preamble"
      (is (= "#!/usr/bin/env node\nvar preamble1 = require(\"preamble1\");\n"
             (make-preamble {:target :nodejs
                             :preamble ["cljs/preamble1.js"]})))))
  (testing "preamble"
    (is (= "var preamble1 = require(\"preamble1\");\nvar preamble2 = require(\"preamble2\");\n"
           (make-preamble {:preamble ["cljs/preamble1.js"
                                      "cljs/preamble2.js"]})))))

(deftest test-check-sourcemap
  (testing "optimizations none"
    (is (check-source-map {:source-map true :optimizations :none}))
    (is (check-source-map {:source-map false :optimizations :none}))
    (is (thrown? AssertionError (check-source-map {:source-map "target/build/app.js.map" :optimizations :none})))))

(deftest test-parse-externs
  (testing "var"
    (is (= (parse-externs "var foo = {}; var bar = function(){}")
           [['foo] ['bar]])))
  (testing "var and expression"
    (is (= (parse-externs "var foo = {};foo.bar = {};foo.baz; foo.bam = function(){}")
           [['foo] ['foo 'bar] ['foo 'baz] ['foo 'bam]])))
  (testing "nested var"
    (is (= (parse-externs "var foo = {bar: {'baz': function(){}}};")
           [['foo] ['foo 'bar] ['foo 'bar 'baz]])))
  (testing "nested var and expression"
    (is (= (parse-externs "var foo = {};foo.bar = {'baz': {bam: {}}};")
           [['foo] ['foo 'bar] ['foo 'bar 'baz] ['foo 'bar 'baz 'bam]]))))

(deftest test-generate-externs
  (let [user-env '{:ns {:name cljs.user} :locals {}}]
    (testing "no existing externs"
      (is (= (env/with-compiler-env (env/default-compiler-env {})
               (ana/analyze user-env '(.bar js/foo))
               (generate-externs {} []))
             "var foo = {};foo.bar = function(){};"))
      (is (= (env/with-compiler-env (env/default-compiler-env {})
               (ana/analyze user-env '(.-bam js/foo.bar))
               (generate-externs {} []))
             "var foo = {};foo.bar = {};foo.bar.bam = {};"))
      (is (= (env/with-compiler-env (env/default-compiler-env {})
               (ana/analyze user-env '(do (js/FooBar.)
                                          (new js/BarFoo)))
               (generate-externs {} []))
               "var BarFoo = {};var FooBar = {};")))
    (testing "existing externs"
      (is (= (env/with-compiler-env (env/default-compiler-env {})
               (ana/analyze user-env '(do (.-bar js/foo)
                                          (.baz js/window)))
               (generate-externs {} ["var baz = function(){};"]))
               "var foo = {};var window = {};foo.bar = {};window.baz = function(){};")))))
