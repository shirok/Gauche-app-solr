;;; -*- coding: utf-8 -*-
;;;
;;; Test app.solr
;;;

;; This only tests the features that doesn't actually need to connect to
;; Solr server.

(use gauche.test)
(use sxml.tools)

(test-start "app.solr")
(use app.solr)
(test-module 'app.solr)

(define *sample-result*
  '(*TOP*
    (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
    (response
     (lst
      (|@| (name "responseHeader"))
      (int (|@| (name "status")) "0")
      (int (|@| (name "QTime")) "51")
      (lst (|@| (name "params")) (str (|@| (name "score")) "true")
           (str (|@| (name "q")) "*:*") (str (|@| (name "fl")) "*")))
     (result
      (|@| (start "0") (numFound "17") (name "response"))
      (doc
       (arr (|@| (name "features")) (str "No accents here") (str "这是一个功能")
            (str "This is a feature (translated)") (str "这份文件是很有光泽")
            (str "This document is very shiny (translated)"))
       (str (|@| (name "id")) "GB18030TEST")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "name")) "Test with some GB18030 encoded characters")
       (float (|@| (name "price")) "0.0"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "hard drive"))
       (arr
        (|@| (name "features"))
        (str "7200RPM, 8MB cache, IDE Ultra ATA-133")
        (str
         "NoiseGuard, SilentSeek technology, Fluid Dynamic Bearing (FDB) motor"))
       (str (|@| (name "id")) "SP2514N")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "manu")) "Samsung Electronics Co. Ltd.")
       (date (|@| (name "manufacturedate_dt")) "2006-02-13T15:26:37Z")
       (str (|@| (name "name"))
            "Samsung SpinPoint P120 SP2514N - hard drive - 250 GB - ATA-133")
       (int (|@| (name "popularity")) "6")
       (float (|@| (name "price")) "92.0")
       (str (|@| (name "store")) "35.0752,-97.032"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "hard drive"))
       (arr (|@| (name "features")) (str "SATA 3.0Gb/s, NCQ") (str "8.5ms seek")
            (str "16MB cache"))
       (str (|@| (name "id")) "6H500F0")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "manu")) "Maxtor Corp.")
       (date (|@| (name "manufacturedate_dt")) "2006-02-13T15:26:37Z")
       (str (|@| (name "name"))
            "Maxtor DiamondMax 11 - hard drive - 500 GB - SATA-300")
       (int (|@| (name "popularity")) "6")
       (float (|@| (name "price")) "350.0")
       (str (|@| (name "store")) "45.17614,-93.87341"))
      (doc (arr (|@| (name "cat")) (str "electronics") (str "connector"))
           (arr (|@| (name "features")) (str "car power adapter, white"))
           (str (|@| (name "id")) "F8V7067-APL-KIT")
           (bool (|@| (name "inStock")) "false") (str (|@| (name "manu")) "Belkin")
           (date (|@| (name "manufacturedate_dt")) "2005-08-01T16:30:25Z")
           (str (|@| (name "name")) "Belkin Mobile Power Cord for iPod w/ Dock")
           (int (|@| (name "popularity")) "1") (float (|@| (name "price")) "19.95")
           (str (|@| (name "store")) "45.17614,-93.87341")
           (float (|@| (name "weight")) "4.0"))
      (doc (arr (|@| (name "cat")) (str "electronics") (str "connector"))
           (arr (|@| (name "features")) (str "car power adapter for iPod, white"))
           (str (|@| (name "id")) "IW-02") (bool (|@| (name "inStock")) "false")
           (str (|@| (name "manu")) "Belkin")
           (date (|@| (name "manufacturedate_dt")) "2006-02-14T23:55:59Z")
           (str (|@| (name "name")) "iPod & iPod Mini USB 2.0 Cable")
           (int (|@| (name "popularity")) "1") (float (|@| (name "price")) "11.5")
           (str (|@| (name "store")) "37.7752,-122.4232")
           (float (|@| (name "weight")) "2.0"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "music"))
       (arr
        (|@| (name "features"))
        (str "iTunes, Podcasts, Audiobooks")
        (str "Stores up to 15,000 songs, 25,000 photos, or 150 hours of video")
        (str "2.5-inch, 320x240 color TFT LCD display with LED backlight")
        (str "Up to 20 hours of battery life")
        (str "Plays AAC, MP3, WAV, AIFF, Audible, Apple Lossless, H.264 video")
        (str
         "Notes, Calendar, Phone book, Hold button, Date display, Photo wallet, Built-in games, JPEG photo playback, Upgradeable firmware, USB 2.0 compatibility, Playback speed control, Rechargeable capability, Battery level indication"
         ))
       (str (|@| (name "id")) "MA147LL/A")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "includes")) "earbud headphones, USB cable")
       (str (|@| (name "manu")) "Apple Computer Inc.")
       (date (|@| (name "manufacturedate_dt")) "2005-10-12T08:00:00Z")
       (str (|@| (name "name")) "Apple 60 GB iPod with Video Playback Black")
       (int (|@| (name "popularity")) "10")
       (float (|@| (name "price")) "399.0")
       (str (|@| (name "store")) "37.7752,-100.0232")
       (float (|@| (name "weight")) "5.5"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "memory"))
       (arr (|@| (name "features"))
            (str "CAS latency 2,\t2-3-3-6 timing, 2.75v, unbuffered, heat-spreader"))
       (str (|@| (name "id")) "TWINX2048-3200PRO")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "manu")) "Corsair Microsystems Inc.")
       (date (|@| (name "manufacturedate_dt")) "2006-02-13T15:26:37Z")
       (str (|@| (name "name"))
            "CORSAIR  XMS 2GB (2 x 1GB) 184-Pin DDR SDRAM Unbuffered DDR 400 (PC 3200) Dual Channel Kit System Memory - Retail"
            )
       (str (|@| (name "payloads")) "electronics|6.0 memory|3.0")
       (int (|@| (name "popularity")) "5")
       (float (|@| (name "price")) "185.0")
       (str (|@| (name "store")) "37.7752,-122.4232"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "memory"))
       (str (|@| (name "id")) "VS1GB400C3")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "manu")) "Corsair Microsystems Inc.")
       (date (|@| (name "manufacturedate_dt")) "2006-02-13T15:26:37Z")
       (str (|@| (name "name"))
            "CORSAIR ValueSelect 1GB 184-Pin DDR SDRAM Unbuffered DDR 400 (PC 3200) System Memory - Retail"
            )
       (str (|@| (name "payloads")) "electronics|4.0 memory|2.0")
       (int (|@| (name "popularity")) "7")
       (float (|@| (name "price")) "74.99")
       (str (|@| (name "store")) "37.7752,-100.0232"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "memory"))
       (arr (|@| (name "features")) (str "CAS latency 3,\t 2.7v"))
       (str (|@| (name "id")) "VDBDB1A16")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "manu")) "A-DATA Technology Inc.")
       (date (|@| (name "manufacturedate_dt")) "2006-02-13T15:26:37Z")
       (str (|@| (name "name"))
            "A-DATA V-Series 1GB 184-Pin DDR SDRAM Unbuffered DDR 400 (PC 3200) System Memory - OEM"
            )
       (str (|@| (name "payloads")) "electronics|0.9 memory|0.1")
       (int (|@| (name "popularity")) "0")
       (str (|@| (name "store")) "45.17614,-93.87341"))
      (doc
       (arr (|@| (name "cat")) (str "electronics") (str "monitor"))
       (arr
        (|@| (name "features"))
        (str
         "30\" TFT active matrix LCD, 2560 x 1600, .25mm dot pitch, 700:1 contrast"
         ))
       (str (|@| (name "id")) "3007WFP")
       (bool (|@| (name "inStock")) "true")
       (str (|@| (name "includes")) "USB cable")
       (str (|@| (name "manu")) "Dell, Inc.")
       (str (|@| (name "name")) "Dell Widescreen UltraSharp 3007WFP")
       (int (|@| (name "popularity")) "6")
       (float (|@| (name "price")) "2199.0")
       (str (|@| (name "store")) "43.17614,-90.57341")
       (float (|@| (name "weight")) "401.6"))))))

(test* "solr-response->result-count" '(17 0 10)
       (receive xs (solr-result->response-count *sample-result*) xs))

(test* "solr-response->doc-nodes" 10
       (let1 nodes (solr-result->doc-nodes *sample-result*)
         (and (every (^n (eq? (sxml:name n) 'doc)) nodes)
              (length nodes))))

(test-end)
