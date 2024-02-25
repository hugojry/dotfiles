-- Clojure mappings
vim.keymap.set('n', '<localleader>e', 'cp', { remap = true, buffer = true })
vim.keymap.set('n', '<localleader>f', 'cpaF', { remap = true , buffer = true})
vim.keymap.set('n', '<localleader>k', ':%Eval<cr>', { buffer = true })

-- fireplace is using these mappings, so take them back
vim.api.nvim_create_autocmd('User', {
    pattern = 'FireplaceActivate',
    callback = function()
        vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { buffer = true })
        vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { buffer = true })
    end
})
