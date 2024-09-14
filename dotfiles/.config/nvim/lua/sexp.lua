local M = {}
local a = vim.api
local ts = vim.treesitter

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
---@return TSNode
local function find_sexp(node)
  while not sexp_node_types[node:type()] do
    local parent = node:parent()
    if parent then
      node = parent
    else
      return node
    end
  end
  return node
end

local function cur_order(row_a, col_a, row_b, col_b)
  return (row_a == row_b and col_b - col_a) or row_b - row_a
end

---@param node TSNode
---@return TSNode[]?
local function is_between(node, row, col)
  local child_iter = node:iter_children()
  local prev

  for child in child_iter do
    local start_row, start_col, end_row, end_col = child:range()

    -- Then the cursor is between children of the current node
    if cur_order(row, col, start_row, start_col) > 0 then
      return { prev, child }
    -- Then the cursor falls inside one of the children of the current node
    elseif cur_order(row, col, end_row, end_col) > 0 then
      return
    end

    prev = child
  end
end

local exclusive_modes = { v = true, n = true, x = true }

function M.adjacent_sexp(is_forward)
  local row, col = unpack(a.nvim_win_get_cursor(0))
  row = row - 1

  local node = ts.get_node()
  if not node then return end

  -- Find a node that represents a type of sexp
  node = find_sexp(node)

  -- If the cursor is between two siblings of the current node then move
  -- to the start of the adjacent sibling
  local nodes = is_between(node, row, col)
  if nodes then
    local before, after = unpack(nodes)
    local dest_row, dest_col = (is_forward and after or before):range()
    a.nvim_win_set_cursor(0, { dest_row + 1, dest_col })
    return
  end

  if not is_forward then
    local node_start_row, node_start_col = node:range()
    if row ~= node_start_row or col ~= node_start_col then
      a.nvim_win_set_cursor(0, { node_start_row + 1, node_start_col })
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

  a.nvim_win_set_cursor(0, { dest_row + 1, dest_col })
end

M.forward_sexp = function()
  local count = vim.v.count == 0 and 1 or vim.v.count
  for _ = 1, count do M.adjacent_sexp(true) end
end
M.backward_sexp = function()
  local count = vim.v.count == 0 and 1 or vim.v.count
  for _ = 1, count do M.adjacent_sexp(false) end
end

return M
