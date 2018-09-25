-- Localize global functions to prevent a changing environment
-- from messing with this code. This should allow multiple
-- instances of this code to run at once. (Don't do that...)
local setfenv = setfenv
local getfenv = getfenv
local setmetatable = setmetatable
local type = type
local rawequal = rawequal
local select = select
local tostring = tostring
local newproxy = newproxy

local defaultMT = {}
function defaultMT:__index(k) 		return self[k] 	end
function defaultMT:__newindex(k, v) self[k] = v 	end
function defaultMT:__call(...)		self(...) 		end
function defaultMT:__concat(v)		return self .. v end
function defaultMT:__unm()			return -self 	end
function defaultMT:__add(v)			return self + v end
function defaultMT:__sub(v)			return self - v end
function defaultMT:__mul(v)			return self * v end
function defaultMT:__div(v)			return self / v end
function defaultMT:__mod(v)			return self % v end
function defaultMT:__pow(v)			return self ^ v end
function defaultMT:__tostring()		return tostring(self) end
function defaultMT:__eq(v)			return self == v end
function defaultMT:__lt(v)			return self < v end
function defaultMT:__le(v)			return self <= v end
function defaultMT:__len()			return #self 	end

local Capsule = {}
Capsule.__metatable = 'This debug metatable is locked.'
setmetatable(Capsule, {__index = print})

-- We don't want to prevent interfaces and 'data' from being
-- garbage collected. While unwrapped 'data' shouldn't be present
-- in lua, interfaces will always be present in lua for the
-- lifetime of the data. So, we remove the data when the interfaces
-- are garbage collected, and allow interfaces to be collected.
local datas = setmetatable({}, {__mode = 'k'})
local wrapped = setmetatable({}, {__mode = 'v'})

local i, n 
-- Since RBXLua isn't multithreaded and wrap/unwrap cannot
-- yield, i and n are fine being passed as upvalues. This
-- allows a very simple implmentation of a function wrapper
-- without the use of tables and unpack, which does not
-- always preserve nil values.

local function unwrap(...)
	if not i then
		i = 1
		n = select('#', ...)
	end

	local v = select(i, ...)
	if wrapped[v] then
		v = datas[wrapped[v]]
	end

	if i < n then
		return v, unwrap(i + 1, n, ...)
	else
		i = nil
		n = nil
		return v
	end
end

local function wrap(...)
	if not i then
		i = 1
		n = select('#', ...)
	end

	local v = select(i, ...)
	if not wrapped[v] then
		local vType = type(v)
		local interface

		if vType == 'function' then
			interface = function(...)
				return wrap(value(unwrap(...)))
			end
		elseif vType == 'table' then
			interface = Capsule.new(v, {})
		elseif vType == 'userdata' then
			interface = Capsule.new(v, newproxy(true))
		end

		if interface then
			wrapped[v] = interface
			wrapped[interface] = interface
			datas[interface] = v
			v = interface
		end
	end

	if i < n then
		return v, unwrap(i + 1, n, ...)
	else
		i = nil
		n = nil
		return v
	end
end

function Capsule.new(data, interface)
	if wrapped[data] then
		return wrapped[data]
	end

	setmetatable(interface, Capsule)
	datas[interface] = data
	wrapped[data] = interface -- Same data, same capsule. Preserves equality tests.
	wrapped[interface] = interface -- Prevent encapsulating capsules.

	return interface
end

return function()
	return setfenv(2, wrap(getfenv(2)))
end