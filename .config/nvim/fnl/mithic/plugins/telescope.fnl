(local _this ...)

(var telescope-ext [])

[{1 "nvim-telescope/telescope.nvim"
  :dependencies ["nvim-lua/plenary.nvim"
                 "natecraddock/telescope-zf-native.nvim"
                 "nvim-telescope/telescope-file-browser.nvim"]
  :lazy false
  :cmd [:Telescope]
  :config #(let [telescope (require :telescope)
                 fb-actions (require (.. _this :.fb_actions))]
            ((. (require :lazy) :load) {:plugins [:nvim-treesitter]})
            (telescope.setup
               {:extensions
                 {:file_browser
                   {:cwd_to_path false
                    :grouped false
                    :files true
                    :add_dirs true
                    :depth 1
                    :auto_depth false
                    :select_buffer true
                    :hidden true
                    :respect_gitignore false
                    :no_ignore true
                    :follow_symlinks true
                    :collapse_dirs false
                    :dir_icon "D"
                    :dir_icon_hl :Default
                    :display_stat {:date true :mode true :size true}
                    :hijack_netrw true
                    :use_fd true
                    :git_status true
                    :hide_parent_dir false
                    :prompt_path true
                    :quiet false
                    :mappings
                      {:i
                        {"<A-Cr>" fb-actions.mithic_rifle
                         "<Bs>" fb-actions.backspace
                         "<C-Cr>" fb-actions.change_cwd
                         "<C-h>" fb-actions.toggle_hidden
                         "<C-w>" fb-actions.goto_cwd
                         "<S-Cr>" fb-actions.create_from_prompt
                         "<A-c>" false "<A-d>" false "<A-m>" false "<A-r>" false
                         "<A-y>" false "<C-e>" false "<C-f>" false "<C-g>" false
                         "<C-o>" false "<C-s>" false}
                       :n
                        {"<A-Cr>" fb-actions.mithic_rifle
                         "<Bs>" fb-actions.backspace
                         "<C-Cr>" fb-actions.change_cwd
                         "<C-h>" fb-actions.toggle_hidden
                         "<C-w>" fb-actions.goto_cwd
                         "<S-Cr>" fb-actions.create_from_prompt
                         "<localleader>R" fb-actions.move
                         "<localleader>d" fb-actions.mithic_dragon_drop
                         "<localleader>o" fb-actions.create
                         "<localleader>r" fb-actions.rename
                         "<localleader>x" fb-actions.mithic_trash
                         "<localleader>y" fb-actions.copy
                         "u" fb-actions.goto_parent_dir
                         "~" fb-actions.goto_home_dir}}}
                  :zf-native
                   {:file {:enable true
                           :highlight_results true
                           :match_filename true}
                    :generic
                      {:enable true
                       :highlight_results true
                       :match_filename false}}}
                :pickers {:find_files
                          {:find_command
                           ["fd" "--type=f" "--color=never" "--hidden"
                            "--no-require-git" "--exclude=.git"]}}})
            (each [_ ext (ipairs telescope-ext)]
              (telescope.load_extension ext)))
  :keys
    (let [builtin (fn [cmd args] #((. (require :telescope.builtin) cmd) args))]
      [["<leader> " (builtin :find_files)]
       ["<leader>," (builtin :buffers)]
       ["<leader><"
        (builtin :find_files
                  {:find_command ["fd" "--type=f" "--color=never" "--hidden"
                                  "--no-ignore"]})]
       ["<leader>/" (builtin :live_grep)]
       ["<leader><C-p>" (builtin :registers)]
       ["<leader>h" (builtin :help_tags)]
       ["<leader>'" (builtin :marks)]
       ["<leader>."
        #(let [telescope (require :telescope)]
           (telescope.extensions.file_browser.file_browser))]
       ["<leader>>"
        #(let [telescope (require :telescope)]
           (telescope.extensions.file_browser.file_browser {:path "%:p:h"}))]])}


 {1 "natecraddock/telescope-zf-native.nvim"
  :init #(table.insert telescope-ext :zf-native)}


 {1 "nvim-telescope/telescope-file-browser.nvim"
  :init #(table.insert telescope-ext :file_browser)}]
