# My personal KISS Emacs config.

# Prerequisites

- Emacs 27+ (27.0.91 is recommended)
- Gnu `find`

# intro

## KISS

Keep It Simple & Stupid

### 1. require-package

``` cl
(defun require-package (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
```

This was taken from [Purcell][1]'s config.

### 2. macro `after` of `with-eval-after-load`

`with-eval-after-load` lets you defer execution of code until after a feature has been loaded.  This is used extensively throughout the config, so wrapping macro has been written for ease of use.  This is what keeps the config loading fast.

Another useful feature is that it can also be used to run code if a package has been installed by using `-autoloads`; e.g.

```cl
(after 'magit
  ;; execute after magit has been loaded
  )
(after "magit-autoloads"
  ;; execute if magit is installed/available
  )
(after [evil magit]
  ;; execute after evil and magit have been loaded
  )
```

Here is [definition][2] of `after`. This was taken from [bling][3].

### 3. recursively finds anythings and load it

At the bottom of the `init.el` is the following gem:

``` cl
(cl-loop for file in (reverse (directory-files-recursively config-directory "\\.el$"))
  do (load file)))
```

Basically, it recursively finds anything in `config/` and loads it.  If you want to add additional configuration for a new language, simply create `new-language.el` in `config/` and it will automatically be loaded.  Files are loaded in reverse order so that any functions defined will be available in child nodes.

This was taken from [bling][3].

### 4. others

#### bindings in one place

Another decision is to keep all keybindings in one place: `/bindings/**/*.el`.

This was taken from [bling][3].

#### lazy installation of major mode packages

By combining `after`, `require-package`, and `auto-mode-alist`, packages are installed only when necessary.  If you never open a Javascript file, none of those packages will be installed.

```cl
(defmacro /boot/lazy-major-mode (pattern mode)
  "Defines a new major-mode matched by PATTERN, installs MODE if necessary, and activates it."
  `(add-to-list 'auto-mode-alist
                '(,pattern . (lambda ()
                               (require-package (quote ,mode))
                               (,mode)))))
```

This was taken from [bling][3].

# install

```
# initialize submodules in the clone
git clone --recursive https://github.com/han1475/emacs.d.git ~/.emacs.d
```

# tips

here be dragons.

# license

MIT


[1]: https://github.com/purcell/emacs.d
[2]: https://github.com/han1475/emacs.d/blob/master/core/core-boot.el#L30-L50
[3]: https://github.com/bling/dotemacs
