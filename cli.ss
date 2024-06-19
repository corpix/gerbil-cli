(import :std/format
        :std/iter
        :std/pregexp
        (for-syntax :std/format
                    (only-in :std/srfi/1 list-index)))
(export main)

(def current-cli-version (make-parameter (void)))
(def current-cli-app (make-parameter (void)))
(def current-cli-global-flags (make-parameter (void)))

(defclass NodeCtx (decl parent)
  transparent: #t)

(defclass TypeNode (ctx value)
  transparent: #t)
(defmethod {value TypeNode}
  (lambda (self)
    (@ self value)))

(defclass FlagNode (ctx name value)
  transparent: #t)
(defmethod {value FlagNode}
  (lambda (self)
    (let (type-node (@ self value))
      {type-node.value})))
(defmethod {command FlagNode}
  (lambda (self)
    (let find-parent ((node self))
      (let (parent (@ node ctx parent))
        (if (or (CommandNode? parent)
                (not parent))
          parent
          (find-parent parent))))))
(defclass Flag (aliases type description usage readonly? value action)
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
                   (aliases (@ self aliases))
                   (alias (find (cut equal? arg-name <>) aliases))
                   (type (@ self type))
                   (node (FlagNode
                          ctx: (NodeCtx decl: self parent: parent)))
                   ((values value-node args)
                    (if alias {type.consume (if arg-value (cons arg-value (cdr args))
                                                (cdr args))
                                            node}
                        (values #f args))))
              (values (and alias
                           (begin0 node
                             (set! (@ node name) (car aliases))
                             (when value-node
                               (when (@ self readonly?)
                                 (error (format "can not set value for flag ~a because it is marked as readonly"
                                                alias)))
                               (set! (@ node value) value-node))))
                      args))
            (values #f args)))
        (values #f args)))))

(defclass FlagsNode (ctx childs)
  transparent: #t)
(defmethod {alist FlagsNode}
  (lambda (self)
    (map (lambda (node)
           (cons (@ node name)
                 {node.value}))
         (@ self childs))))
(defclass Flags (list)
  transparent: #t)
(defmethod {consume Flags}
  (lambda (self args (parent #f))
    (let* ((global-flags (current-cli-global-flags))
           (flags (append (if (void? global-flags) []
                              (@ global-flags list))
                          (@ self list)))
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
                        (set! (@ node childs) acc))
                      next))))
        (values #f args)))))

;;

(defclass CommandNode (ctx name flags child)
  transparent: #t)
(defmethod {run! CommandNode}
  (lambda (self args)
    (let ((child (@ self child))
          (flags (@ self flags)))
      (for (flag (or (and flags (@ flags childs)) []))
        (let (action (@ flag ctx decl action))
          (and action (action flag))))
      (if child
        {child.run! args}
        (let (action (@ self ctx decl action))
          (unless action
            (error (format "action for ~a command is not defined" (@ self name))))
          (action self args))))))
(defmethod {flags CommandNode}
  (lambda (self)
    (let (flags (@ self flags))
      (if flags {flags.alist} []))))
(defmethod {flag CommandNode}
  (lambda (self name)
    (let loop ((command-node self))
      (let* ((parent (let find-parent ((node command-node))
                       (let (parent (@ node ctx parent))
                         (if (or (CommandNode? parent)
                                 (not parent))
                           parent
                           (find-parent parent)))))
             (flags-decl (@ command-node ctx decl flags))
             (flag-decl (find (lambda (decl) (find (cut equal? name <>) (@ decl aliases)))
                              (or (and flags-decl (@ flags-decl list)) [])))
             (flags (@ command-node flags))
             (flag-value (if flag-decl
                           (let* ((flag-node (find (lambda (node)
                                                     (eq? (@ node ctx decl) flag-decl))
                                                   (or (and flags (@ flags childs)) []))))
                             (if flag-node {flag-node.value}
                                 (@ flag-decl value))))))
        (if (and (void? flag-value) parent)
          (loop parent)
          flag-value)))))
(defclass Command (name description usage flags action commands)
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
                 ((values child-node args)
                  (if commands {commands.consume args node}
                      (values #f args))))
            (values (begin0 node
                      (set! (@ node child) child-node)
                      (set! (@ node flags) flags-node))
                    args))
          (values #f args)))
      (values #f args))))
(defmethod {run! Command}
  (lambda (self args)
    (let (((values node rest) {self.consume args}))
      (unless node
        (error (format "unexpected input: ~a" args)))
      {node.run! rest})))

(defclass Commands (list)
  transparent: #t)
(defmethod {consume Commands}
  (lambda (self args (parent #f))
    (if (pair? args)
      (let* ((name (car args))
             (command (find (lambda (command)
                              (equal? name (@ command name)))
                            (@ self list)))
             (node (CommandNode ctx: (NodeCtx decl: command parent: parent)))
             ((values child-node args)
              (if command {command.consume args node}
                  (values #f args))))
        (if child-node
          (values (begin0 node
                    (set! (@ node child) child-node))
                  args)
          (values #f args)))
      (values #f args))))

;;

(defclass Cli (name description command)
  transparent: #t)
(defmethod {run! Cli}
  (lambda (self args)
    (let (command (@ self command))
      (parameterize ((current-cli-app self))
        {command.run! (cons (command-name) args)}))))

;;

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

(defsyntax (defflag-type stx)
  (syntax-case stx ()
    ((macro (name parameters ...))
     #'(defstruct name (parameters ...)
         transparent: #t))
    ((macro name) #'(defflag-type (name)))))

(defsyntax (~defflags-aux stx)
  (def (stx-flag-type macro acc keyword value)
    (cond
     ((eq? (syntax->datum keyword) type:)
      (append acc [keyword
                   (if (pair? (syntax->datum value)) value
                       (with-syntax ((type-constructor value)) #'(type-constructor)))]))
     (else (append acc [keyword value]))))
  (syntax-case stx ()
    ((macro (alias0 alias* ...)
       slots ...)
     (with-syntax (((flag-slots ...)
                    (fold-n (cut stx-flag-type #'macro <...>) []
                            (syntax->list #'(slots ...)))))
       #'(Flag
          aliases: (map symbol->string '(alias0 alias* ...))
          type: (String)
          value: (void)
          flag-slots ...)))))

(defsyntax (defflags stx)
  (syntax-case stx ()
    ((macro id (flag-decl ...) ...)
     #'(def id (Flags list: [(~defflags-aux flag-decl ...) ...])))))

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

(def (display-version-action ctx)
  (displayln (current-cli-version))
  (exit 0))

(def (display-help-action ctx)
  (def command {ctx.command})
  (def (format-flags flags)
    (string-join (map (lambda (flag)
                        (format "  ~a~a\t\t~a"
                                (string-join (@ flag aliases) ", ")
                                (if (@ flag usage) (string-append " " (@ flag usage)) "")
                                (@ flag description))
                        )
                      flags)
                 "\n"))
  (displayln (format "\n  ~a - ~a\n\nUsage:\n  ~a ~a~a~a~a~a"
                     (@ (current-cli-app) name)
                     (@ (current-cli-app) description)
                     (@ command name)
                     (or (@ command ctx decl usage)
                         (string-join (append (if (@ command ctx decl commands) ["[command]"] [])
                                              (if (@ command ctx decl flags) ["[options]"] []))
                                      " "))
                     (if (@ command ctx decl description)
                       (string-append "\n\n  " (@ command ctx decl description))
                       "")
                     (if (@ command ctx decl commands)
                       (format "\n\nAvailable Commands:\n~a"
                               (string-join (map (lambda (subcommand)
                                                   (format "  ~a\t\t~a"
                                                           (@ subcommand name)
                                                           (or (@ subcommand description) "")))
                                                 (@ command ctx decl commands list))
                                            "\n"))
                       "")
                     (if (@ command ctx decl flags)
                       (format "\n\nOptions:\n~a" (format-flags (@ command ctx decl flags list)))
                       "")
                     (if (current-cli-global-flags)
                       (format "\n\nGlobal Options:\n~a"
                               (format-flags (@ (current-cli-global-flags) list)))
                       "")))
  (exit 0))


(defflags global-flags
  ((-v --version)
   description: "Show application version"
   type: Boolean
   action: display-version-action)
  ((-h --help)
   description: "Show command line interface help"
   type: Boolean
   action: display-help-action))
;; todo: implement merge! and/or append! methods on Flags?
(current-cli-global-flags global-flags)

;;

(defflags root-flags
  ((--verbose)
   description: "Verbose mode"
   type: Boolean
   value: 1))

(defflags hello-flags
  ((-c --config)
   description: "Load configuration file"
   value: "./config.ss"))

(defcommand (hello-command flags: hello-flags
                           description: "Say hello"
                           name: "hello")
  (lambda (ctx args)
    (displayln (format "hello from hello command, args: ~a, flags: ~a"
                       args {ctx.flags}))
    (displayln (list 'config {ctx.flag "--config"}))
    (displayln (list 'config-short {ctx.flag "-c"}))
    (displayln (list 'verbose {ctx.flag "--verbose"}))
    (displayln (list 'unknown {ctx.flag "--oops"}))))

(defcli (app
         name: "app"
         description: "Test app"
         flags: root-flags
         commands: [hello-command])
  (lambda (ctx args)
    (displayln (format "hello from cli root, args: ~a" args))))

(def (main . args)
  {app.run! args})
