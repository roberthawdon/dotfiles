(beacon-mode 1)

(map! :leader
      :desc "Toggle beacon mode" "t B" #'beacon-mode)

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs globally" "t c" #'centaur-tabs-mode
      :desc "Toggle tabs local display" "t C" #'centaur-tabs-local-mode
      :desc "Toggle tab group view" "t t" #'centaur-tabs-toggle-groups)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
                                               (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)

(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq fancy-splash-image (concat doom-private-dir "images/emacs-logo.png"))

(setq doom-theme 'doom-ir-black)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package emojify
  :hook (after-init . global-emojify-mode))
(map! :leader
      :desc "Toggle emojify" "t e" #'global-emojify-mode)

(defun rh/ex-kill-buffer-and-close ()
  (interactive)
  (unless (char-equal (elt (buffer-name) 0) ?*)
    (kill-this-buffer))
  )

(defun rh/ex-save-kill-buffer-and-close ()
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  )

(evil-ex-define-cmd "q[uit]" 'rh/ex-kill-buffer-and-close )
(evil-ex-define-cmd "wq" 'rh/ex-save-kill-buffer-and-close )

;; (when window-system
;;  (if (> (display-pixel-height) 1080)
;;    (setq doom-font (font-spec :family "VictorMono Nerd Font" :size 24)
;;          doom-variable-pitch-font (font-spec :family "Annai MN" :size 30)
;;          doom-big-font (font-spec :family "VictorMono Nerd Font" :size 48)))
;;  (if (<= (display-pixel-height) 1080)
;;    (setq doom-font (font-spec :family "VictorMono Nerd Font" :size 12)
;;          doom-variable-pitch-font (font-spec :family "Annai MN" :size 15)
;;          doom-big-font (font-spec :family "VictorMono Nerd Font" :size 24))))
(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Annai MN" :size 15)
      doom-big-font (font-spec :family "VictorMono Nerd Font" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(map! :leader
      (:prefix ("k" . "kubernetes")
       :desc "Config" "." #'kele-config
       :desc "Get" "g" #'kele-get
       :desc "Kele mode" "k" #'kele-mode
       :desc "List" "l" #'kele-list
       (:prefix ("c" . "context")
       :desc "Switch context" "s" #'kele-context-switch)))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
;;(after! doom-themes
;;  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree"  "d n" #'neotree-dir)

(map! :leader
      :desc "Magit pull"   "g p" #'magit-pull
      :desc "Magit push"   "g P" #'magit-push
      :desc "Magit diff"   "g d" #'magit-diff
      :desc "Magit log"    "g L" #'magit-log ;; Override Doom Emacs's default
      :desc "Magit rebase" "g r" #'magit-rebase ;; Override Doom Emacs's default
      :desc "Magit reset"  "g R" #'magit-reset) ;; Override Doom Emacs's default

(setq auth-sources '("~/.authinfo"))
(map! :leader
      :desc "Forge add repository" "g a" #'forge-add-repository)

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq minimap-window-location 'right)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle minimap-mode" "m" #'minimap-mode))

(set-face-attribute 'mode-line nil :font "VictorMono Nerd Font")
(setq doom-modeline-height 12     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit start.org (start page)" "=" #'(lambda () (interactive) (find-file "~/.config/doom/start.org"))
       :desc "Edit doom config.org"        "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el"           "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el"       "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))
       (:prefix ("a" . "Edit agendas")
       :desc "Edit work agenda"            "w" #'(lambda () (interactive) (find-file "~/Org/agendas/work.org"))
       :desc "Edit personal agenda"        "p" #'(lambda () (interactive) (find-file "~/Org/agendas/personal.org")))))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq org-directory "~/Org/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        ;; org-log-done 'time
        org-log-done 'note
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-table-convert-region-max-lines 20000
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "TO REVIEW(e)"      ; A change that is ready to be reviewed
             "IN PROGRESS(i)"    ; A tast that is in progress
             "PROJ(p)"           ; A project that contains other tasks
             "BLOG(b)"           ; Blog writing assignments
             "DOCUMENT(o)"       ; Document writing assignments (split sections into tasks)
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "APPROVED(a)"       ; Change has been approved
             "REJECTED(r)"       ; Change has been rejected
             "CANCELLED(c)" )))) ; Task has been cancelled

(after! org
  (setq org-agenda-files '("~/Org/agendas/")))

(after! org-fancy-priorities
  (setq
     ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
     ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
     ;; org-fancy-priorities-list '("🟥" "🟧" "🟨")
     org-fancy-priorities-list '((?A . "❗ [Highest Priority]")
                                  (?B . "🟥 [High Priority]")
                                  (?C . "🟨 [Medium Priority]")
                                  (?D . "🟩 [Low Priority]")
                                  (?E . "☕ [Non Priority]"))
     org-priority-faces
     '((?A :foreground "#ff0000" :weight bold)
       (?B :foreground "#ff6c6b" :weight bold)
       (?C :foreground "#97d138" :weight bold)
       (?D :foreground "#25be4b" :weight bold)
       (?E :foreground "#c678dd" :weight bold))
     org-agenda-block-separator 8411)

  (setq org-agenda-custom-commands
        '(("v" "A better agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'todo))
                   (org-agenda-overriding-header "Highest-priority unfinished tasks:")))
            (tags "PRIORITY=\"B\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'todo))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'todo))
                   (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
            (tags "PRIORITY=\"D\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'todo))
                   (org-agenda-overriding-header "Low-priority unfinished tasks:")))
            (tags "PRIORITY=\"E\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'todo))
                   (org-agenda-overriding-header "Non-priority unfinished tasks:")))
            (tags "customtag"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'todo))
                   (org-agenda-overriding-header "Tasks marked with customtag:")))

            (agenda "")
            (alltodo ""))))))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(defun rh/ginsert-auto-tangle-tag ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert auto_tangle tag" "i a" #'rh/ginsert-auto-tangle-tag)

(use-package ox-clip)
(use-package ox-man)
(use-package ox-gemini)

(after! org

  (defun rh/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun rh/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (rh/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun rh/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (rh/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun rh/org-set-last-modified ()
    "Update the LAST-MODIFIED file property in the preamble."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (rh/org-set-time-file-property "LAST-MODIFIED")))

  (add-hook 'before-save-hook #'rh/org-set-last-modified)

)

(after! org
    (defun rh/gorg-colors-doom-one ()
    "Enable Doom One colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#51afef" ultra-bold)
            (org-level-2 1.6 "#c678dd" extra-bold)
            (org-level-3 1.5 "#98be65" bold)
            (org-level-4 1.4 "#da8548" semi-bold)
            (org-level-5 1.3 "#5699af" normal)
            (org-level-6 1.2 "#a9a1e1" normal)
            (org-level-7 1.1 "#46d9ff" normal)
            (org-level-8 1.0 "#ff6c6b" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-dracula ()
    "Enable Dracula colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#8be9fd" ultra-bold)
            (org-level-2 1.6 "#bd93f9" extra-bold)
            (org-level-3 1.5 "#50fa7b" bold)
            (org-level-4 1.4 "#ff79c6" semi-bold)
            (org-level-5 1.3 "#9aedfe" normal)
            (org-level-6 1.2 "#caa9fa" normal)
            (org-level-7 1.1 "#5af78e" normal)
            (org-level-8 1.0 "#ff92d0" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-gruvbox-dark ()
    "Enable Gruvbox Dark colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#458588" ultra-bold)
            (org-level-2 1.6 "#b16286" extra-bold)
            (org-level-3 1.5 "#98971a" bold)
            (org-level-4 1.4 "#fb4934" semi-bold)
            (org-level-5 1.3 "#83a598" normal)
            (org-level-6 1.2 "#d3869b" normal)
            (org-level-7 1.1 "#d79921" normal)
            (org-level-8 1.0 "#8ec07c" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-monokai-pro ()
    "Enable Monokai Pro colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#78dce8" ultra-bold)
            (org-level-2 1.6 "#ab9df2" extra-bold)
            (org-level-3 1.5 "#a9dc76" bold)
            (org-level-4 1.4 "#fc9867" semi-bold)
            (org-level-5 1.3 "#ff6188" normal)
            (org-level-6 1.2 "#ffd866" normal)
            (org-level-7 1.1 "#78dce8" normal)
            (org-level-8 1.0 "#ab9df2" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-nord ()
    "Enable Nord colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#81a1c1" ultra-bold)
            (org-level-2 1.6 "#b48ead" extra-bold)
            (org-level-3 1.5 "#a3be8c" bold)
            (org-level-4 1.4 "#ebcb8b" semi-bold)
            (org-level-5 1.3 "#bf616a" normal)
            (org-level-6 1.2 "#88c0d0" normal)
            (org-level-7 1.1 "#81a1c1" normal)
            (org-level-8 1.0 "#b48ead" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-oceanic-next ()
    "Enable Oceanic Next colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#6699cc" ultra-bold)
            (org-level-2 1.6 "#c594c5" extra-bold)
            (org-level-3 1.5 "#99c794" bold)
            (org-level-4 1.4 "#fac863" semi-bold)
            (org-level-5 1.3 "#5fb3b3" normal)
            (org-level-6 1.2 "#ec5f67" normal)
            (org-level-7 1.1 "#6699cc" normal)
            (org-level-8 1.0 "#c594c5" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-palenight ()
    "Enable Palenight colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#82aaff" ultra-bold)
            (org-level-2 1.6 "#c792ea" extra-bold)
            (org-level-3 1.5 "#c3e88d" bold)
            (org-level-4 1.4 "#ffcb6b" semi-bold)
            (org-level-5 1.3 "#a3f7ff" normal)
            (org-level-6 1.2 "#e1acff" normal)
            (org-level-7 1.1 "#f07178" normal)
            (org-level-8 1.0 "#ddffa7" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-solarized-dark ()
    "Enable Solarized Dark colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#268bd2" ultra-bold)
            (org-level-2 1.6 "#d33682" extra-bold)
            (org-level-3 1.5 "#859900" bold)
            (org-level-4 1.4 "#b58900" semi-bold)
            (org-level-5 1.3 "#cb4b16" normal)
            (org-level-6 1.2 "#6c71c4" normal)
            (org-level-7 1.1 "#2aa198" normal)
            (org-level-8 1.0 "#657b83" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-solarized-light ()
    "Enable Solarized Light colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#268bd2" ultra-bold)
            (org-level-2 1.6 "#d33682" extra-bold)
            (org-level-3 1.5 "#859900" bold)
            (org-level-4 1.4 "#b58900" semi-bold)
            (org-level-5 1.3 "#cb4b16" normal)
            (org-level-6 1.2 "#6c71c4" normal)
            (org-level-7 1.1 "#2aa198" normal)
            (org-level-8 1.0 "#657b83" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

    (defun rh/gorg-colors-tomorrow-night ()
    "Enable Tomorrow Night colors for Org headers."
    (interactive)
    (dolist
        (face
        '((org-level-1 1.7 "#81a2be" ultra-bold)
            (org-level-2 1.6 "#b294bb" extra-bold)
            (org-level-3 1.5 "#b5bd68" bold)
            (org-level-4 1.4 "#e6c547" semi-bold)
            (org-level-5 1.3 "#cc6666" normal)
            (org-level-6 1.2 "#70c0ba" normal)
            (org-level-7 1.1 "#b77ee0" normal)
            (org-level-8 1.0 "#9ec400" normal)))
        (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
        (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  ;; Load our desired rh/gorg-colors-* theme on startup
  (rh/gorg-colors-doom-one)
)

(setq org-journal-dir "~/Org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

(setq   org-highest-priority ?A
    org-default-priority ?C
    org-lowest-priority ?E
)

(after! org
  (setq org-roam-directory "~/Org/roam/")
  (if  (eq system-type 'darwin)
        (setq org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"))
  (if  (eq system-type 'gnu/linux)
        (setq org-roam-graph-viewer "brave")))

(map! :leader
      (:prefix ("n r" . "org-roam")
       :desc "Completion at point" "c" #'completion-at-point
       :desc "Find node"           "f" #'org-roam-node-find
       :desc "Show graph"          "g" #'org-roam-graph
       :desc "Insert node"         "i" #'org-roam-node-insert
       :desc "Capture to node"     "n" #'org-roam-capture
       :desc "Toggle roam buffer"  "r" #'org-roam-buffer-toggle))

(setq org-roam-capture-templates
  '(("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>.org"
                        "#+title: ${title}\n#+CREATED: %U\n#+LAST-MODIFIED: %U\n")
     :unnarrowed t)))

(defun rh/ginsert-auto-publish ()
  "Insert auto-tangle tag in a literate config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+filetags: publish")
  (evil-force-normal-state))

(map! :leader
      :desc "Insert publish tag" "i P" #'rh/ginsert-auto-publish)

(defun rh/force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))

(map! :leader
      :desc "Switch to perspective NAME"       "DEL" #'persp-switch
      :desc "Switch to buffer in perspective"  "," #'persp-switch-to-buffer
      :desc "Switch to next perspective"       "]" #'persp-next
      :desc "Switch to previous perspective"   "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
      :desc "Remove perspective by name"       "-" #'persp-remove-by-name)

;; (setq initial-buffer-choice "~/.config/doom/start.org")
;;
;; (define-minor-mode start-mode
;;   "Provide functions for custom start page."
;;   :lighter " start"
;;   :keymap (let ((map (make-sparse-keymap)))
;;           ;;(define-key map (kbd "M-z") 'eshell)
;;             (evil-define-key 'normal start-mode-map
;;               (kbd "1") '(lambda () (interactive) (find-file "~/.config/doom/config.org"))
;;               (kbd "2") '(lambda () (interactive) (find-file "~/.config/doom/init.el"))
;;               (kbd "3") '(lambda () (interactive) (find-file "~/.config/doom/packages.el"))
;;               (kbd "4") '(lambda () (interactive) (find-file "~/.config/doom/eshell/aliases"))
;;               (kbd "5") '(lambda () (interactive) (find-file "~/.config/doom/eshell/profile")))
;;           map))
;;
;; (add-hook 'start-mode-hook 'read-only-mode) ;; make start.org read-only; use 'SPC t r' to toggle off read-only.
;; (provide 'start-mode)

(setq frame-title-format '("%b – Emacs"))
