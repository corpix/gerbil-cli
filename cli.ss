(import :std/format
        :std/iter
        :std/pregexp
        (for-syntax (only-in :std/srfi/1 list-index)))
(export main)

;;

(defclass TypeNode (definition value)
  transparent: #t)
(defsyntax (defflag-type stx)
  (syntax-case stx ()
    ((macro (name parameters ...))
     #'(defstruct name (parameters ...)
         transparent: #t))
    ((macro name) #'(defflag-type (name)))))

;;

(defflag-type String)
(defmethod {consume String}
  (lambda (self args)
    (values (TypeNode
             definition: self
             value: (car args))
            (cdr args))))

(defflag-type Number)
(defmethod {consume Number}
  (lambda (self args)
    (values (TypeNode
             definition: self
             value: (string->number (car args)))
            (cdr args))))

(defflag-type Boolean)
(defmethod {consume Boolean}
  (lambda (self args)
    (values (TypeNode
             definition: self
             value: #t)
            args)))

(defflag-type (ListOf subtype))
(defmethod {consume ListOf}
  (lambda (self args)
    (let (subtype (@ self subtype))
      (values (TypeNode
               definintion: self
               value: (map (lambda (item)
                             (let (((values node _)
                                    {subtype.consume [item]}))
                               node))
                           (string-split (car args) ",")))
              (cdr args)))))

;;

(defclass FlagNode (definition name value)
  transparent: #t)
(defclass Flag (aliases type usage value)
  transparent: #t)
(defmethod {consume Flag}
  (let (flag-regex (pregexp "^(-{1,2}[a-zA-Z0-9][a-zA-Z0-9-]*)(?:=(.+))?"))
    ;; (pregexp-match flag-regex "-foo=hello")
    ;; => ("-foo=hello" "-foo" "hello")
    (lambda (self args)
      (if (pair? args)
        (let (arg* (pregexp-match flag-regex (car args)))
          (if arg*
            (let* ((name (cadr arg*))
                   (value (caddr arg*))
                   (alias (find (cut equal? name <>) (@ self aliases)))
                   (type (@ self type))
                   ((values value-node args)
                    (if alias {type.consume (if value (cons value (cdr args)) (cdr args))}
                        (values #f args))))
              (values (and alias value-node
                           (FlagNode
                            definition: self
                            name: alias
                            value: value-node))
                      args))
            (values #f args)))
        (values #f args)))))

(defclass FlagsNode (definition value)
  transparent: #t)
(defclass Flags (list)
  transparent: #t)
(defmethod {consume Flags}
  (lambda (self args)
    (let (flags (@ self list))
      (if (and (pair? flags) (pair? args))
        (let loop ((acc [])
                   (current (car args))
                   (next (cdr args)))
          (let* ((current+next (cons current next))
                 ((values node next)
                  (let lp ((flag (car flags))
                           (rest-flags (cdr flags)))
                    (let (((values node args) {flag.consume current+next}))
                      (if (or (not (pair? rest-flags)) node)
                        (values node args)
                        (lp (car rest-flags) (cdr rest-flags))))))
                 (acc (if node (cons node acc) acc)))
            (if (and node (pair? next))
              (loop acc
                    (car next)
                    (cdr next))
              (values (FlagsNode
                       definition: self
                       value: acc)
                      next))))
        (values #f args)))))

(defsyntax (~defflags-aux stx)
  (syntax-case stx ()
    ((macro (alias0 alias* ...)
       slots ...)
     #'(Flag
        aliases: (map symbol->string '(alias0 alias* ...))
        type: (String)
        value: (void)
        slots ...))))

(defsyntax (defflags stx)
  (syntax-case stx ()
    ((macro id (flag-definition ...) ...)
     #'(def id (Flags list: [(~defflags-aux flag-definition ...) ...])))))

(defflags root-flags
  ((-c --config)
   usage: "Load configuration file"
   value: "./config.ss"))

;;

(defclass CommandNode (definition name flags commands)
  transparent: #t)
(defclass Command (name usage flags action commands)
  transparent: #t)
(defmethod {consume Command}
  (lambda (self args)
    (if (pair? args)
      (let (name (@ self name))
        (if (equal? name (car args))
          (let* ((flags (@ self flags))
                 (commands (@ self commands))
                 ((values flags-node args)
                  (if flags {flags.consume (cdr args)}
                      (values #f args)))
                 ((values commands-node args)
                  (if commands {commands.consume args}
                      (values #f args)))
                 )
            (values (CommandNode
                     definition: self
                     name: name
                     flags: flags-node
                     commands: commands-node)
                    args))
          (values #f args)))
      (values #f args))))

(defclass CommandsNode (definition value)
  transparent: #t)
(defclass Commands (list)
  transparent: #t)
(defmethod {consume Commands}
  (lambda (self args)
    (if (pair? args)
      (let* ((name (car args))
             (command (find (lambda (command)
                              (equal? name (@ command name)))
                            (@ self list)))
             ((values value-node args)
              (if command {command.consume args}
                  (values #f args))))
        (if command
          (values (CommandsNode
                   definition: self
                   value: value-node)
                  args)
          (values #f args)))
      (values #f args))))

(defsyntax (defcommand stx)
  (syntax-case stx ()
    ((macro (id slots ...)
       proc)
     (let* ((slots (syntax->datum #'(slots ...)))
            (commands-index (list-index (lambda (item) (eq? item commands:)) slots)))
       (with-syntax (((slots ...)
                      (if commands-index
                        (let (((values head tail) (split-at slots commands-index)))
                          (unless (> (length tail) 1)
                            (error "expected commands keyword to have value"))
                          (list-set! tail 1
                                     (with-syntax ((commands (list-ref tail 1)))
                                       #'(Commands list: commands)))
                          (append head tail))
                        slots)))
         #'(def id
             (Command
              name: (symbol->string 'id)
              action: proc
              slots ...)))))))

(defcommand (hello-command flags: root-flags
                           name: "hello")
  (lambda (ctx . args)
    (displayln "hello")))

(defcommand (root flags: root-flags
                  commands: [hello-command])
  (lambda (ctx . args)
    (displayln (list ctx args))))

;;

(output-port-readtable-set! (current-output-port)
                            (readtable-sharing-allowed?-set
                             (output-port-readtable (current-output-port)) #f))
(let (((values node args)
       {root.consume '("root" "-c" "foo" "--config=./hello" "-c=./hello2"
                       "hello" "-c" "bar")}))
  (pretty-print (list node args)))

;;

;; (defclass Cli (name description command commands)
;;   transparent: #t)
;; (defmethod {run! Cli}
;;   (lambda (self args)
;;     (let ((command (@ self command)))
;;       (unless command
;;         (error "command is not specified"))
;;       {command.run! args})))

;; (defsyntax (defcli stx)
;;   (syntax-case stx ()
;;     ((macro id slots ...)
;;      #'(def id (Cli slots ...)))))

;; (defcli app
;;   name: "app"
;;   description: "Description of the app"
;;   command: root
;;   commands: [hello-action])

;; {app.run! '("-c" "hello")}

;; (memf (lambda (v) (eq? v 'foo)) '(bar baz foo qux))

;; ;;

;; (def (main . args)
;;   )