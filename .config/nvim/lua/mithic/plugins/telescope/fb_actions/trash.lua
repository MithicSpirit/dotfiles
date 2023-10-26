--[[ Adapted from remove action ]]

local fb_utils = require "telescope._extensions.file_browser.utils"
local action_state = require "telescope.actions.state"

local Path = require "plenary.path"
local async = require "plenary.async"

local function get_confirmation(opts, callback)
  local fb_config = require "telescope._extensions.file_browser.config"
  if fb_config.values.use_ui_input then
    opts.prompt = opts.prompt .. " [y/N] "
    vim.ui.input(opts, function(input)
      callback(input and input:lower() == "y")
    end)
  else
    async.run(function()
      return vim.fn.confirm(opts.prompt, table.concat({ "&Yes", "&No" }, "\n"), 2) == 1
    end, callback)
  end
end

return function(prompt_bufnr)
  local current_picker = action_state.get_current_picker(prompt_bufnr)
  local finder = current_picker.finder
  local quiet = current_picker.finder.quiet
  local selections = fb_utils.get_selected_files(prompt_bufnr, true)
  if vim.tbl_isempty(selections) then
    fb_utils.notify("actions.trash", { msg = "No selection to be trashed!", level = "WARN", quiet = quiet })
    return
  end

  local files = vim.tbl_map(function(sel)
    return sel.filename:sub(#sel:parent().filename + 2)
  end, selections)

  for _, sel in ipairs(selections) do
    if sel:is_dir() then
      local abs = sel:absolute()
      local msg
      if finder.files and Path:new(finder.path):parent():absolute() == abs then
        msg = "Parent folder cannot be trashed!"
      end
      if not finder.files and Path:new(finder.cwd):absolute() == abs then
        msg = "Current folder cannot be trashed!"
      end
      if msg then
        fb_utils.notify("actions.trash", { msg = msg .. " Prematurely aborting.", level = "WARN", quiet = quiet })
        return
      end
    end
  end

  local trashed = {}

  local message = "Selections to be trashed: " .. table.concat(files, ", ")
  fb_utils.notify("actions.trash", { msg = message, level = "INFO", quiet = quiet })
  -- TODO fix default vim.ui.input and nvim-notify 'selections to be deleted' message
  get_confirmation({ prompt = "Trash selection? (" .. #files .. " items)" }, function(confirmed)
    vim.cmd [[ redraw ]] -- redraw to clear out vim.ui.prompt to avoid hit-enter prompt
    if confirmed then
      for _, p in ipairs(selections) do
        local is_dir = p:is_dir()
        local cmd = {"rmtrash", "-rf", "--", p:absolute()}
        vim.fn.system(cmd)
        if vim.v.shell_error == 0 then
          if not is_dir then
            fb_utils.delete_buf(p:absolute())
          else
            fb_utils.delete_dir_buf(p:absolute())
          end
          table.insert(trashed, p.filename:sub(#p:parent().filename + 2))
        else
          local msg = "Command failed: " .. table.concat(cmd, " ")
          fb_utils.notify("actions.trash", { msg = msg, level = "WARN", quiet = quiet })
        end
      end
      fb_utils.notify(
        "actions.trash",
        { msg = "Trashed: " .. table.concat(trashed, ", "), level = "INFO", quiet = quiet }
      )
      current_picker:refresh(current_picker.finder)
    else
      fb_utils.notify("actions.trash", { msg = "Trashing selections aborted!", level = "INFO", quiet = quiet })
    end
  end)
end
