return {
    "nvim-lualine/lualine.nvim",
    requires = { "kyazdani42/nvim-web-devicons", opt = true }, -- optional
    config = function()
        require("lualine").setup {
            options = {
                theme = 'powerline_dark', -- or any theme you prefer
                -- section_separators = '',
                -- component_separators = '',
            },
        }
    end,
}
