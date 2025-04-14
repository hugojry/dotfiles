local M = {}
local a = vim.api
local ts = vim.treesitter
local sexp = require('sexp')
local pos_order = sexp.pos_order
local get_cur = sexp.get_cur
local set_cur = sexp.set_cur

local sexp_node_types = {
  list_lit = true,
  vec_lit  = true,
  map_lit  = true,
  kwd_lit  = true,
  sym_lit  = true,
  num_lit  = true,
  str_lit  = true
}

---@param node TSNode
---@return TSNode[]?
local function is_between(node, row, col)
  local child_iter = node:iter_children()
  local prev

  for child in child_iter do
    local start_row, start_col, end_row, end_col = child:range()

    -- Then the cursor is between children of the current node
    if pos_order(row, col, start_row, start_col) > 0 then
      return { prev, child }
    -- Then the cursor falls inside one of the children of the current node
    elseif pos_order(row, col, end_row, end_col) > 0 then
      return
    end

    prev = child
  end
end

function M.adjacent_sexp(is_forward)
  local row, col = get_cur()

  local node = ts.get_node()
  if not node then return end

  -- Find a node that represents a type of sexp
  node = sexp.find_node_types(sexp_node_types, node)
  if not node then return end

  -- If the cursor is between two siblings of the current node then move
  -- to the start of the adjacent sibling
  local nodes = is_between(node, row, col)
  if nodes then
    local dest_row, dest_col = nodes[is_forward and 2 or 1]:range()
    a.nvim_win_set_cursor(0, { dest_row + 1, dest_col })
    return
  end

  if not is_forward then
    local node_start_row, node_start_col = node:range()
    if row ~= node_start_row or col ~= node_start_col then
      assert(node_start_row)
      assert(node_start_col)
      set_cur(node_start_row, node_start_col)
      return
    end
  end

  local dest_row, dest_col
  local f = is_forward and 'next_named_sibling' or 'prev_named_sibling'
  local sib = node[f](node)
  if not sib then
    if is_forward then
      _, _, dest_row, dest_col = node:range()
    else
      dest_row, dest_col = node:range()
    end
    if is_forward and a.nvim_get_mode().mode ~= 'no' then
      dest_col = dest_col - 1
    end
  else
    dest_row, dest_col = sib:range()
  end

  assert(dest_row)
  assert(dest_col)
  set_cur(dest_row, dest_col)
end

M.forward_sexp = function()
  for _ = 1, vim.v.count1 do M.adjacent_sexp(true) end
end

M.backward_sexp = function()
  for _ = 1, vim.v.count1 do M.adjacent_sexp(false) end
end

return M
