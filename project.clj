;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(defproject com.7theta/tar-clj "0.1.0"
  :url "https://github.com/7theta/tar-clj"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [com.7theta/utilis "1.13.0"]
                 [org.apache.commons/commons-compress "1.21"]]
  :profiles {:dev {:dependencies []
                   :source-paths ["dev"]}}
  :scm {:name "git"
        :url "https://github.com/7theta/tar-clj"})
