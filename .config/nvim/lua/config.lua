--[[
  _____                              __
 / ___/__  __ _  __ _  ___ ____  ___/ /__
/ /__/ _ \/  ' \/  ' \/ _ `/ _ \/ _  (_-<
\___/\___/_/_/_/_/_/_/\_,_/_//_/\_,_/___/
--]]
-- Space-Tab Conversion
vim.api.nvim_create_user_command(
	'SpacesToTabs',
	function(tbl)
		vim.opt.expandtab = false
		local previous_tabstop = vim.bo.tabstop
		vim.opt.tabstop = tonumber(tbl.args)
		vim.api.nvim_command 'retab!'
		vim.opt.tabstop = previous_tabstop
	end,
	{force = true, nargs = 1}
)

vim.api.nvim_create_user_command(
	'TabsToSpaces',
	function(tbl)
		vim.opt.expandtab = true
		local previous_tabstop = vim.bo.tabstop
		vim.opt.tabstop = tonumber(tbl.args)
		vim.api.nvim_command 'retab'
		vim.opt.tabstop = previous_tabstop
	end,
	{force = true, nargs = 1}
)

