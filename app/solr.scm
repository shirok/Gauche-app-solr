;;;
;;; app.solr - Solr binding
;;;
;;;   Copyright (c) 2011  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(define-module app.solr
  (use gauche.dictionary)
  (use srfi-1)
  (use srfi-19)
  (use rfc.http)
  (use rfc.uri)
  (use util.list)
  (use util.match)
  (use file.util)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.tools)
  (use sxml.serializer)

  (export <solr> <solr-error>
          solr-add solr-add* solr-commit solr-optimize solr-rollback
          solr-delete
          solr-query solr-result->response-count solr-result->doc-nodes
          solr-result->doc-alist)
  )
(select-module app.solr)

(define-class <solr> ()
  ((uri :init-keyword :uri) ; solr endpoint url e.g. http://localhost:8983/solr
   ))

(define-method write-object ((obj <solr>) port)
  (format port "#<solr ~a>" (~ obj'uri)))

(define-condition-type <solr-error> <error> #f
  (status) (headers) (body))

;;
;; Updating
;;

(define (solr-add solr doc :key (commit #f) (overwrite #t))
  (post-request solr "update" 'POST
                `(add (@ (overwrite ,(xbool overwrite))) ,(record->sxml doc))
                `((commit ,(xbool commit)))))

(define (solr-add* solr docs :key (commit #f) (overwrite #t))
  (post-request solr "update" 'POST
                `(add (@ (overwrite ,(xbool overwrite)))
                      ,@(map record->sxml docs))
                `((commit ,(xbool commit)))))

(define (solr-commit solr :key (wait-flush #t) (wait-searcher #t)
                               (expunge-deletes #f))
  (post-request solr "update" 'POST
                `(commit (@ (waitFlush ,(xbool wait-flush))
                            (waitSearcher ,(xbool wait-searcher))
                            (expungeDeletes ,(xbool expunge-deletes))))))

(define (solr-optimize solr :key (wait-flush #t) (wait-searcher #t)
                                 (max-segments 1))
  (post-request solr "update" 'POST
                `(optimize (@ (waitFlush ,(xbool wait-flush))
                              (waitSearcher ,(xbool wait-searcher))
                              (maxSegments ,max-segments)))))

(define (solr-rollback solr)
  (post-request solr "update" 'POST '(rollback)))

(define (solr-delete solr :key (ids '()) (queries '()) (commit #f))
  (post-request solr "update" 'POST
                `(delete ,@(map (^i `(id ,(x->string i))) ids)
                         ,@(map (^q `(query ,(x->string q))) queries))
                `((commit ,(xbool commit)))))

;;
;; Querying
;;

;; API
(define (solr-query solr :key (query "*:*")
                              (fields "*")
                              (search-name "select")
                              (score #t)
                              (sort #f)
                              (params '()))
  (post-request solr search-name 'GET #f
                `((q ,query) (fl ,(canon-fields fields))
                  (score ,(xbool score))
                  ,@(cond-list [sort `(sort ,sort)])
                  ,@(append-map (^p (map (^v `(,(car p) ,(value->sxml v)))
                                         (cdr p)))
                                params))))

(define (canon-fields fields)
  (if (list? fields)
    (string-join (map x->string fields) ",")
    (x->string fields)))

;; API
(define solr-result->doc-nodes (sxpath '(// doc)))

;; API
(define (solr-result->doc-alist sxml)
  (map sxml-record->alist (solr-result->doc-nodes sxml)))

;; API
;;  returns total # of hits, start count and # of nodes in the response.
(define (solr-result->response-count sxml)
  (if-let1 node (extract-response-node sxml)
    (values (sxml:num-attr node 'numFound)
            (sxml:num-attr node 'start)
            (length (sxml:content node)))))

(define extract-response-node
  (if-car-sxpath '(// "result[attribute::name='response']")))

(define (sxml-record->alist doc)
  (map (^n (cons (string->symbol (sxml-field->key n)) (sxml-field->value n)))
       (sxml:child-nodes doc)))

(define sxml-field->key (if-car-sxpath '(@ name *text*)))

(define (sxml-field->value node) ;; returns [SXML] -> Obj
  (define (convert children)
    (case (sxml:node-name node)
      [(str)   (x->string (car children))]
      [(arr)   (map sxml-field->value children)]
      [(int)   (x->integer (car children))]
      [(float) (x->number (car children))]
      [(bool)  (not (equal? (car children) "false"))]
      [(date)  (string->date (car children) "~Y-~m-~dT~H:~M:~S~z")]
      ;; TODO: have we exhausted all possible types?
      [else (error "couldn't convert field node:" node)]))
  (convert (sxml:child-nodes node)))
  
;;
;; Utilities
;;

(define (post-request solr path method body :optional (params '()))
  (receive (host ppath secure?) (decompose-uri (~ solr'uri))
    ;; If we post lots of requests in short time period, we may use up
    ;; local ports faster than the system reclaims them.  If we get into
    ;; the situation (EADDRNOTAVAIL) we wait for a bit and retry.
    ;; Returns (status headers body)
    (define (do-request)
      (or (guard (e [(and (<system-error> e) (eqv? (~ e'errno) EADDRNOTAVAIL))
                     (sys-nanosleep #e1e8)
                     #f])
            (call-with-values
                (cut http-request method host
                     (http-compose-query (build-path ppath path) params)
                     :sender (if body
                               (http-string-sender
                                (srl:sxml->xml-noindent body))
                               (http-null-sender))
                     :secure secure?
                     :content-type "text/xml")
              list))
          (do-request)))
    (apply parse-response (do-request))))

(define (parse-response status headers body)
  (rlet1 sxml (call-with-input-string body (cut ssax:xml->sxml <> '()))
    (unless (equal? status "200")
      (errorf <solr-error> :status status :headers :headers :body sxml
              "Solr error: ~s" sxml))))

(define (update-endpoint solr :optional (query-params '()))
  (http-compose-query (~ solr'uri) query-params 'utf-8))

;; returns (server:port path secure?)
(define (decompose-uri uri)
  (receive (scheme user host port path query frag) (uri-parse uri)
    (values (if port #`",|host|:,|port|" host)
            path
            (equal? scheme "https") )))

;; rec can be a <dictionary> or an alist
;; returns SXML
(define (record->sxml rec)
  `(doc ,@(reverse
           (cond [(good-alist? rec)
                  (fold (^(p s) (field->sxml (car p) (cdr p) s)) '() rec)]
                 [(is-a? rec <dictionary>) (dict-fold rec field->sxml '())]
                 [else (error "Invalid object to add (must be a <dictionary> \
                               or an alist)" rec)]))))

(define (field->sxml key val seed)
  (if (list? val)
    (fold (^(v s) (field->sxml key v s)) seed val)
    `((field (@ (name ,(x->string key))) ,(value->sxml val)) ,@seed)))

;; for <date>, ensure we use UTC.
(define (value->sxml val)
  (cond
   [((any-pred real? string? symbol?) val) (x->string val)]
   [(boolean? val) (xbool val)]
   [(date? val) (date->string (time-utc->date (date->time-utc val) 0) "~4")]
   [else (errorf "Couldn't convert Scheme value ~a to Solr value." val)]))

(define (good-alist? rec)
  (and (list? rec)
       (every (^p (and (pair? p) (symbol? (car p)))) rec)))

(define (xbool val) (if val "true" "false"))
