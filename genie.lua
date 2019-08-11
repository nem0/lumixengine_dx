project "renderer"
	files { 
		"src/**.c",
		"src/**.cpp",
		"src/**.h",
		"genie.lua"
	}
	excludes {
		"../../src/renderer/ffr/ffr.cpp"
	}

if build_studio then
	project "studio"
		for conf,conf_dir in pairs({Debug="release", RelWithDebInfo="release"}) do
			for platform,target_platform in pairs({win="windows", linux="linux", }) do
				configuration { "x64", conf, target_platform }
				libdirs {"../dx11/external/lib/" .. platform .. "64" .. "_" .. binary_api_dir .. "/" .. conf_dir}
			end
			configuration {}
		end
end