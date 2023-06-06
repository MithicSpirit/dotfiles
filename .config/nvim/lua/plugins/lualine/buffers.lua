local lualine_buffers = require('lualine.components.buffers')

local exclude_buftype = {
	quickfix = true
}
local exclude_filetype = {
	leaninfo = true
}

local function include_buffer(b)
	return vim.fn.buflisted(b) ~= 0
		and not exclude_buftype[vim.api.nvim_buf_get_option(b, 'buftype')]
		and not exclude_filetype[vim.api.nvim_buf_get_option(b, 'filetype')]
end

function lualine_buffers:buffers()
	local buffers = {}
	lualine_buffers.bufpos2nr = {}
	lualine_buffers.bufnr2pos = {}
	for b = 1, vim.fn.bufnr('$') do
		if include_buffer(b) then
			buffers[#buffers + 1] = self:new_buffer(b, #buffers + 1)
			lualine_buffers.bufpos2nr[#buffers] = b
			lualine_buffers.bufnr2pos[b] = #buffers
		end
	end

	return buffers
end

return lualine_buffers
