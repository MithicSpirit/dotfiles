local fb_utils = require('telescope._extensions.file_browser.utils')
local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local plenary_job = require('plenary.job')

local extras = {}


function extras.fb_dragon_drop(prompt_bufnr)
	local quiet = action_state.get_current_picker(prompt_bufnr).finder.quiet
	local selections = fb_utils.get_selected_files(prompt_bufnr, true)
	if vim.tbl_isempty(selections) then
		fb_utils.notify("actions.open", {
			msg = "No selection available!",
			level = "INFO",
			quiet = quiet,
		})
		return
	end

	local files = {}
	for _, selection in ipairs(selections) do
		files[#files + 1] = selection:absolute()
	end

	plenary_job:new({
		command = 'dragon-drop',
		args = files,
	}):start()
end

function extras.fb_rifle(prompt_bufnr)
	local quiet = action_state.get_current_picker(prompt_bufnr).finder.quiet
	local selections = fb_utils.get_selected_files(prompt_bufnr, true)
	if vim.tbl_isempty(selections) then
		fb_utils.notify("actions.open", {
			msg = "No selection available!",
			level = "INFO",
			quiet = quiet,
		})
		return
	end

	for _, selection in ipairs(selections) do
		plenary_job:new({
			command = 'rifle',
		args = {selection:absolute()},
		}):start()
	end

	actions.close(prompt_bufnr)
end


return extras
