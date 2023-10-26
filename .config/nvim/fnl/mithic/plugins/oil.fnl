[{1 "stevearc/oil.nvim"
  :lazy false
  :enabled false
  :opts
    {:default_file_explorer true
     :columns [:permission :size :mtime]
     :buf_options {:bufhidden :hide :buflisted false}
     :win_options
       {:concealcursor :nvic :conceallevel 3 :cursorcolumn false :foldcolumn :0
        :list false :signcolumn :no :spell false :wrap false}
     :delete_to_trash true
     :trash_command "trash-put"
     :skip_confirm_for_simple_edits false
     :prompt_save_on_select_new_entry true
     :cleanup_delay_ms 2000
     :keymaps {"-" :actions.parent
               "<C-c>" :actions.close
               "<C-h>" :actions.select_split
               "<C-l>" :actions.refresh
               "<C-p>" :actions.preview
               "L" :actions.select_vsplit
               "<C-t>" :actions.select_tab
               "<CR>" :actions.select
               "_" :actions.open_cwd
               "`" :actions.cd
               "g." :actions.toggle_hidden
               "g?" :actions.show_help
               "gs" :actions.change_sort
               "gx" :actions.open_external
               "~" :actions.tcd}
     :use_default_keymaps false
     :view_options {:show_hidden true
                    :is_always_hidden (fn [_name _bufnr] false)
                    :is_hidden_file (fn [name _bufnr] (vim.startswith name "."))
                    :sort [[:type :asc] [:name :asc]]}
     :float {:padding 2
             :border :rounded
             :max_width 0
             :max_height 0
             :win_options {:winblend 0}
             :override (fn [conf] conf)}
     :preview {:max_width 0.9
               :min_width [40 0.4]
               :width nil
               :max_height 0.9
               :min_height [5 0.1]
               :height nil
               :border :rounded
               :win_options {:winblend 0}}
     :progress {:max_width 0.9
                :min_width [40 0.4]
                :width nil
                :max_height [10 0.9]
                :min_height [5 0.1]
                :height nil
                :border :rounded
                :minimized_border :none
                :win_options {:winblend 0}}}}]
