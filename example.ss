(import :std/format
        :cli/cli)
(export main)

(current-cli-version "0.0.1")

(defflags root-flags
  ((--verbose)
   description: "Verbose mode"
   type: Boolean))

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
