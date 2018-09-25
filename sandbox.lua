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

-- Unfortunately metamethods have different ways of invoking
-- the default metamethods of values. This table provides
-- metamethods which does that, so they can be easily wrapped.
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

-- We don't want to prevent interfaces and 'data' from being
-- garbage collected. While unwrapped 'data' shouldn't be present
-- in lua, interfaces will always be present in lua for the
-- lifetime of the data. So, we remove the data when the interfaces
-- are garbage collected, and allow interfaces to be collected.
local original = setmetatable({}, {__mode = 'k'})
local wrapper = setmetatable({}, {__mode = 'v'})

-- Since RBXLua isn't multithreaded and wrap/unwrap cannot
-- yield, i and n are fine being passed as upvalues. This
-- allows a very simple implmentation of a function wrapper
-- without the use of tables and unpack, which does not
-- always preserve nil values.
local i, n 

-- This function is called to make sure no wrappers get passed
-- outside the sandbox. The only data that leaves the sandbox is
-- through metamethod and function call arguments. These arguments
-- are always unwrapped.
local function unwrap(...)
	if not i then
		i = 1
		n = select('#', ...)
	end

	local v = select(i, ...)
	if wrapper[v] then
		v = original[wrapper[v]]
	end

	if i < n then
		return v, unwrap(i + 1, n, ...)
	else
		i = nil
		n = nil
		return v
	end
end

-- The return value of all function and metamethod calls is wrapped.
local function wrap(...)
	if not i then
		i = 1
		n = select('#', ...)
	end

	local v = select(i, ...)
	if not wrapper[v] then
		local vType = type(v)
		local interface

		if vType == 'function' then
			local func = v -- v will be changed here in a bit
			interface = function(...)
				return wrap(func(unwrap(...)))
			end
		elseif vType == 'table' then
			interface = setmetatable({}, Capsule)
		elseif vType == 'userdata' then
			interface = setmetatable(newproxy(true), Capsule)
		end

		if interface then
			wrapper[v] = interface -- Same data, same interface. Preserves equality tests.
			wrapper[interface] = interface -- Prevent encapsulating capsules.
			original[interface] = v -- store the original value to perform operations on and unwrap
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

-- Here, we ensure each metamethod is wrapped.
for key, metamethod in next, defaultMT do
	Capsule[key] = wrap(metamethod)
end

-- mwhaha, now even calling rawset is sandboxed... 
-- the only functions not sandboxed are those included in the
-- locked string metatable (i.e. 'myStringVar:sub(i, j)')
return function()
	return setfenv(2, wrap(getfenv(2)))
end