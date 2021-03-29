newoption {
	trigger = "dx12",
	description = "use dx12 backend"
}

newoption {
	trigger = "nodx",
	description = "do not use any dx backend"
}

if _OPTIONS["nodx"] == nil then
	project "renderer"
		files { 
			"src/**.c",
			"src/**.cpp",
			"src/**.h",
			"genie.lua"
		}
		excludes { "../../src/renderer/gpu/gpu.cpp" }

		if _OPTIONS["dx12"] then
			includedirs {"external/pix/Include/WinPixEventRuntime", "external/include/dx" }
			files { "external/pix/bin/x64/WinPixEventRuntime.dll" }
			copy { "external/pix/bin/x64/WinPixEventRuntime.dll" }
			excludes { "src/gpu_dx.cpp" }
			solution "LumixEngine"
				defines { "LUMIX_DX12" }
		else
			excludes { "src/gpu_dx12.cpp" }
			solution "LumixEngine"
				defines { "LUMIX_DX11" }
		end


	if build_studio then
		project "studio"
			libdirs { "external/lib/win64" .. "_" .. binary_api_dir .. "/release"}
			libdirs { "external/pix/bin/x64" }
	end
	if build_app then
		project "app"
			libdirs { "external/lib/win64" .. "_" .. binary_api_dir .. "/release"}
			libdirs { "external/pix/bin/x64" }
	end
end