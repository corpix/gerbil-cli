(import :std/format
        :std/iter
        :std/pregexp
        (for-syntax :std/format
                    (only-in :std/srfi/1 list-index)))
(export main)

(begin-syntax
  ;; todo: move to more common library
  ;; or find a replacement in std lib, very common operation
  (def (fold-n proc init lst n: (n 2))
    (let* ((len (length lst))
           (rem (remainder len n)))
      (unless (= rem 0)
        (error (format
                "expected list length dividable by ~a with no remainder, got len ~a, remainder ~a"
                n len rem)))
      (if (= len 0) init
          (let loop ((acc init)
                     (current (take lst n))
                     (next (drop lst n)))
            (set! acc (apply proc (cons acc current)))
            (if (pair? next)
              (loop acc
                    (take next n)
                    (drop next n))
              acc))))))

;;

(defclass NodeCtx (decl parent)
  transparent: #t)

;;

(defclass TypeNode (ctx value)
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
  (lambda (self args (parent #f))
    (values (TypeNode
             ctx: (NodeCtx decl: self parent: parent)
             value: (car args))
            (cdr args))))

(defflag-type Number)
(defmethod {consume Number}
  (lambda (self args (parent #f))
    (values (TypeNode
             ctx: (NodeCtx decl: self parent: parent)
             value: (string->number (car args)))
            (cdr args))))

(defflag-type Boolean)
(defmethod {consume Boolean}
  (lambda (self args (parent #f))
    (values (TypeNode
             ctx: (NodeCtx decl: self parent: parent)
             value: #t)
            args)))

(defflag-type (ListOf subtype))
(defmethod {consume ListOf}
  (lambda (self args (parent #f))
    (let (subtype (@ self subtype))
      (values (let (node (TypeNode ctx: (NodeCtx decl: self parent: parent)))
                (begin0 node
                  (set! (@ node value)
                    (map (lambda (item)
                           (let (((values node _)
                                  {subtype.consume [item] node}))
                             node))
                         (string-split (car args) ",")))))
              (cdr args)))))

;;

(defclass FlagNode (ctx name value)
  transparent: #t)
(defclass Flag (aliases type usage value)
  transparent: #t)
(defmethod {consume Flag}
  ;; regex to:
  ;; - enforce flags to have `-` or `--` prefix
  ;; - enforce flag name to start from alphanum char following by alphanum with `-`
  ;; - support `--key=value` notation
  (let (flag-regex (pregexp "^(-{1,2}[a-zA-Z0-9][a-zA-Z0-9-]*)(?:=(.+))?"))
    (lambda (self args (parent #f))
      (if (pair? args)
        (let (arg* (pregexp-match flag-regex (car args)))
          (if arg*
            (let* ((arg-name (cadr arg*))
                   (arg-value (caddr arg*))
                   (alias (find (cut equal? arg-name <>) (@ self aliases)))
                   (type (@ self type))
                   (node (FlagNode
                          ctx: (NodeCtx decl: self parent: parent)
                          name: alias))
                   ((values value-node args)
                    (if alias {type.consume (if arg-value (cons arg-value (cdr args))
                                                (cdr args))
                                            node}
                        (values #f args))))
              (values (and alias value-node
                           (FlagNode
                            ctx: (NodeCtx decl: self parent: parent)
                            name: alias
                            value: value-node))
                      args))
            (values #f args)))
        (values #f args)))))

(defclass FlagsNode (ctx value)
  transparent: #t)
(defclass Flags (list)
  transparent: #t)
(defmethod {consume Flags}
  (lambda (self args (parent #f))
    (let ((flags (@ self list))
          (node (FlagsNode ctx: (NodeCtx decl: self parent: parent))))
      (if (and (pair? flags) (pair? args))
        (let loop ((acc [])
                   (current (car args))
                   (next (cdr args)))
          (let* ((current+next (cons current next))
                 ((values flag-node next)
                  (let lp ((flag (car flags))
                           (rest-flags (cdr flags)))
                    (let (((values flag-node args) {flag.consume current+next node}))
                      (if (or (not (pair? rest-flags)) flag-node)
                        (values flag-node args)
                        (lp (car rest-flags) (cdr rest-flags))))))
                 (acc (if flag-node (cons flag-node acc) acc)))
            (if (and flag-node (pair? next))
              (loop acc
                    (car next)
                    (cdr next))
              (values (begin0 node
                        (set! (@ node value) acc))
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
    ((macro id (flag-decl ...) ...)
     #'(def id (Flags list: [(~defflags-aux flag-decl ...) ...])))))

(defflags root-flags
  ((-c --config)
   usage: "Load configuration file"
   value: "./config.ss"))

;;

(defclass CommandNode (ctx name value flags)
  transparent: #t)
(defclass Command (name usage flags action commands)
  transparent: #t)
(defmethod {consume Command}
  (lambda (self args (parent #f))
    (if (pair? args)
      (let (name (@ self name))
        (if (equal? name (car args))
          (let* ((flags (@ self flags))
                 (commands (@ self commands))
                 (node (CommandNode
                        ctx: (NodeCtx decl: self parent: parent)
                        name: name))
                 ((values flags-node args)
                  (if flags {flags.consume (cdr args) node}
                      (values #f args)))
                 ((values commands-node args)
                  (if commands {commands.consume args node}
                      (values #f args))))
            (values (begin0 node
                      (set! (@ node value) commands-node)
                      (set! (@ node flags) flags-node))
                    args))
          (values #f args)))
      (values #f args))))
(defmethod {run! Command}
  (lambda (self node rest (parent #f))
    (let (commands (@ node value))
      (if commands
        {commands.run! node rest parent}
        (let (action (@ self action))
          (unless action
            (error (format "action for ~a command is not defined" (@ self name))))
          (action node rest))))))

(defclass Commands (list)
  transparent: #t)
(defmethod {consume Commands}
  (lambda (self args (parent #f))
    (if (pair? args)
      (let* ((node (CommandNode ctx: (NodeCtx decl: self parent: parent)))
             (name (car args))
             (command (find (lambda (command)
                              (equal? name (@ command name)))
                            (@ self list)))
             ((values value-node args)
              (if command {command.consume args node}
                  (values #f args))))
        (if command
          (values (begin0 node
                    (set! (@ node value) value-node))
                  args)
          (values #f args)))
      (values #f args))))
(defmethod {run! Commands}
  (lambda (self node rest (parent #f))
    (let (command (@ node value))
      {command.run! node rest parent})))

(defsyntax (~defcommand-aux stx)
  (def (stx-wrap-commands acc keyword value)
    (cond
     ((eq? keyword commands:)
      (with-syntax ((commands value))
        (append acc [keyword #'(Commands list: commands)])))
     (else (append acc [keyword value]))))
  (syntax-case stx ()
    ((macro (name slots ...) proc)
     (with-syntax (((command-slots ...)
                    (fold-n stx-wrap-commands [] (syntax->datum #'(slots ...)))))
       #'(Command
          name: name
          action: proc
          command-slots ...)))))

(defsyntax (defcommand stx)
  (syntax-case stx ()
    ((macro (id slots ...) proc)
     #'(def id (~defcommand-aux ((symbol->string 'id) slots ...)
                                proc)))))

(defcommand (hello-command flags: root-flags
                           name: "hello")
  (lambda (ctx . args)
    (displayln "hello")))

;;

;; (output-port-readtable-set! (current-output-port)
;;                             (readtable-sharing-allowed?-set
;;                              (output-port-readtable (current-output-port)) #t))

(defcommand (root-command flags: root-flags
                          commands: [hello-command]
                          name: "root")
  (lambda (ctx . args)
    (displayln "hello")))

(def xxx (void))
(let (((values node args)
       {root-command.consume '("root" "-c" "foo" "--config=./hello" "-c=./hello2"
                               "hello" "-c" "bar")}))
  (set! xxx node)
  (pretty-print (list node args)))

;;

(defclass Cli (name description command)
  transparent: #t)
(defmethod {run! Cli}
  (lambda (self args)
    (let ((command (@ self command))
          ((values node rest) {command.consume (cons (command-name) args) self}))
      (unless node
        (error (format "unexpected input: ~a" args)))
      {command.run! node rest})))

(defsyntax (defcli stx)
  (def (stx-split-command-arguments acc keyword value)
    (begin0 acc
      (let (box (cond
                 ((or (eq? keyword name:)
                      (eq? keyword description:))
                  (car acc))
                 (else (cdr acc))))
        (set-box! box (append (unbox box) [keyword value])))))
  (syntax-case stx ()
    ((macro (id slots ...) proc)
     (let (arguments (fold-n stx-split-command-arguments (cons (box []) (box []))
                             (syntax->datum #'(slots ...))))
       (with-syntax (((cli-slots ...) (unbox (car arguments)))
                     ((command-slots ...) (unbox (cdr arguments))))
         #'(def id (Cli cli-slots ...
                        command: (~defcommand-aux ((command-name) command-slots ...)
                                                  proc))))))))

(defcli (app
         name: "app"
         description: "Description of the app"
         flags: root-flags
         commands: [hello-command])
  (lambda (ctx . args)
    (displayln (list ctx args))))

;; {app.run! '("-c" "hello")}

;; (memf (lambda (v) (eq? v 'foo)) '(bar baz foo qux))

;; ;;

;; (def (main . args)
;;   )