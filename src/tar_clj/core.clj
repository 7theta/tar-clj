;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   MIT License (https://opensource.org/licenses/MIT) which can also be
;;   found in the LICENSE file at the root of this distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(ns tar-clj.core
  (:refer-clojure :exclude [list])
  (:require [clojure.java.io :as io]
            [utilis.fs :as fs])
  (:import [java.io
            ByteArrayInputStream ByteArrayOutputStream
            File FileOutputStream
            PipedInputStream PipedOutputStream]
           [java.net URI]
           [org.apache.commons.compress.archivers ArchiveStreamFactory]
           [org.apache.commons.compress.archivers.tar TarArchiveEntry TarArchiveInputStream TarArchiveOutputStream]
           [org.apache.commons.compress.compressors CompressorStreamFactory]))

(def ^:private compression-method "gz")

(declare ->filename)

(defn archive
  [files & {:keys [directory]}]
  (let [directory (when directory (io/file directory))]
    (with-open [out-stream (ByteArrayOutputStream.)
                compressor-stream (.createCompressorOutputStream
                                   (CompressorStreamFactory.)
                                   compression-method
                                   out-stream)
                ^TarArchiveOutputStream tar-stream (.createArchiveOutputStream
                                                    (ArchiveStreamFactory.)
                                                    ArchiveStreamFactory/TAR
                                                    compressor-stream)]
      (doseq [file files]
        (let [file (io/file file)
              name (.getPath ^URI (if directory
                                    (.relativize (.toURI directory) (.toURI file))
                                    (.toURI file)))
              entry (.createArchiveEntry tar-stream file name)]
          (.putArchiveEntry tar-stream entry)
          (io/copy (io/input-stream file) tar-stream)
          (.closeArchiveEntry tar-stream)))
      (.finish tar-stream)
      (.close tar-stream)
      out-stream)))

(defn archive->bytes
  [^ByteArrayOutputStream archive]
  (.toByteArray archive))

(defn archive->file
  [^ByteArrayOutputStream archive name]
  (let [file (io/file (->filename name))]
    (with-open [file-stream (io/output-stream file)]
      (.writeTo archive file-stream))
    file))

(defn extract
  [^ByteArrayOutputStream archive dest-dir]
  (let [dest-dir (io/file dest-dir)]
    (fs/mkdir dest-dir :recursive true)
    (with-open [in-stream (-> archive .toByteArray ByteArrayInputStream.)
                compressor-stream (.createCompressorInputStream
                                   (CompressorStreamFactory.)
                                   compression-method
                                   in-stream)
                ^TarArchiveInputStream tar-stream (.createArchiveInputStream
                                                   (ArchiveStreamFactory.)
                                                   ArchiveStreamFactory/TAR
                                                   compressor-stream)]
      (loop [count 0]
        (if-let [^TarArchiveEntry entry (.getNextEntry tar-stream)]
          (let [file (io/file (str dest-dir File/separatorChar (.getName entry)))]
            (if (.isDirectory entry)
              (when-not (.exists file)
                (fs/mkdir file :recursive true))
              (io/copy tar-stream file))
            (recur (inc count)))
          {:count count
           :dest-dir dest-dir})))))

(defn list
  [^ByteArrayOutputStream archive]
  (with-open [in-stream (-> archive .toByteArray ByteArrayInputStream.)
              compressor-stream (.createCompressorInputStream
                                 (CompressorStreamFactory.)
                                 compression-method
                                 in-stream)
              ^TarArchiveInputStream tar-stream (.createArchiveInputStream
                                                 (ArchiveStreamFactory.)
                                                 ArchiveStreamFactory/TAR
                                                 compressor-stream)]
    (loop [count 0
           entry-info []]
      (if-let [^TarArchiveEntry entry (.getNextEntry tar-stream)]
        (recur (inc count)
               (conj entry-info {:name (.getName entry)
                                 :size (.getSize entry)
                                 :last-modified (.getLastModifiedDate entry)}))
        {:count count
         :entries entry-info}))))

;;; Implementation

(defn- ->filename
  [archive-name]
  (str archive-name ".tar." compression-method))
