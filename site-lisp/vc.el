;;; vc.el --- Visual Studio 2017 x64 environment       -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <kadode@kadode-PC>
;; Keywords: c

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setenv "CommandPromptType" "Native")
(setenv "ConEmuBackHWND" "0x00020416")
(setenv "ConEmuDrawHWND" "0x0001041A")
(setenv "ConEmuServerPID" "6768")
(setenv "ConEmuTask" "{VS2017}")
(setenv "DevEnvDir" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\")
(setenv "ExtensionSdkDir" "C:\\Program Files (x86)\\Microsoft SDKs\\Windows Kits\\10\\ExtensionSDKs")
(setenv "Framework40Version" "v4.0")
(setenv "FrameworkDir" "C:\\Windows\\Microsoft.NET\\Framework64\\")
(setenv "FrameworkDIR64" "C:\\Windows\\Microsoft.NET\\Framework64")
(setenv "FrameworkVersion" "v4.0.30319")
(setenv "FrameworkVersion64" "v4.0.30319")
(setenv "INCLUDE" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\ATLMFC\\include;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\include;C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\include\\um;C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\ucrt;C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\shared;C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\um;C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\winrt;C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\cppwinrt")
(setenv "LIB" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\ATLMFC\\lib\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\lib\\x64;C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\lib\\um\\x64;C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.16299.0\\ucrt\\x64;C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.16299.0\\um\\x64;")
(setenv "LIBPATH" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\ATLMFC\\lib\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\lib\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\lib\\x86\\store\\references;C:\\Program Files (x86)\\Windows Kits\\10\\UnionMetadata\\10.0.16299.0;C:\\Program Files (x86)\\Windows Kits\\10\\References\\10.0.16299.0;C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319;")
(setenv "NETFXSDKDir" "C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\")
(setenv "Path" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\bin\\HostX64\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\VC\\VCPackages;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\TestWindow;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\TeamFoundation\\Team Explorer;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\MSBuild\\15.0\\bin\\Roslyn;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Team Tools\\Performance Tools\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Team Tools\\Performance Tools;C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Common\\VSPerfCollectionTools\\\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Common\\VSPerfCollectionTools\\;C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.6.1 Tools\\x64\\;C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.16299.0\\x64;C:\\Program Files (x86)\\Windows Kits\\10\\bin\\x64;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\\\MSBuild\\15.0\\bin;C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\;C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\Tools\\;C:\\Program Files\\ConEmu\\ConEmu\\Scripts;C:\\Program Files\\ConEmu;C:\\Program Files\\ConEmu\\ConEmu;C:\\Program Files (x86)\\Common Files\\Oracle\\Java\\javapath;C:\\Program Files\\ImageMagick-7.0.7-Q16;C:\\Python27;C:\\ProgramData\\Oracle\\Java\\javapath;C:\\Program Files (x86)\\Gpg4win\\..\\GnuPG\\bin;C:\\Program Files (x86)\\GnuWin32\\bin;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\bin;C:\\bin\\mpv;C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\Program Files\\MacType;C:\\bin\\emacs\\bin;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Program Files\\Everything;C:\\Users\\kadode\\.cargo\\bin;C:\\bin\\xperf\\;C:\\Program Files (x86)\\Windows Kits\\8.1\\Windows Performance Toolkit\\;C:\\bin\\arbtt\\bin;C:\\bin\\ruby\\bin;C:\\bin\\ffmpeg\\bin;C:\\Program Files\\Git\\cmd;C:\\Program Files (x86)\\GnuWin32\\lib;C:\\bin\\apache-ant-1.10.2\\bin;C:\\bin\\buck\\bin;C:\\Program Files\\Java\\jdk1.8.0_151\\bin;C:\\bin\\kotlinc\\bin;C:\\bin\\gradle-4.5.1\\bin;C:\\Program Files\\Cppcheck;C:\\Program Files\\Git\\bin;C:\\bin\\nc;C:\\Program Files\\Zulu\\zulu-7\\bin\\;C:\\Program Files\\Zulu\\zulu-8\\bin\\;C:\\Program Files\\nodejs\\;C:\\Program Files (x86)\\GtkSharp\\2.12\\bin;C:\\Program Files\\PuTTY\\;C:\\bin\\jflex-1.6.1\\bin;C:\\Program Files\\CMake\\bin;C:\\Program Files (x86)\\LLVM\\bin;C:\\Users\\kadode\\.cargo\\bin;C:\\Program Files (x86)\\mitmproxy\\bin;C:\\Users\\kadode\\AppData\\Roaming\\npm;C:\\Users\\kadode\\AppData\\Local\\Pandoc\\")
(setenv "Platform" "x64")
(setenv "UCRTVersion" "10.0.16299.0")
(setenv "UniversalCRTSdkDir" "C:\\Program Files (x86)\\Windows Kits\\10\\")
(setenv "VCIDEInstallDir" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\VC\\")
(setenv "VCINSTALLDIR" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\")
(setenv "VCToolsInstallDir" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\")
(setenv "VCToolsRedistDir" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Redist\\MSVC\\14.13.26020\\")
(setenv "VCToolsVersion" "14.13.26128")
(setenv "VisualStudioVersion" "15.0")
(setenv "VS150COMNTOOLS" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\Tools\\")
(setenv "VSCMD_ARG_app_plat" "Desktop")
(setenv "VSCMD_ARG_HOST_ARCH" "x64")
(setenv "VSCMD_ARG_TGT_ARCH" "x64")
(setenv "VSCMD_VER" "15.6.7")
(setenv "VSINSTALLDIR" "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\")
(setenv "WindowsLibPath" "C:\\Program Files (x86)\\Windows Kits\\10\\UnionMetadata\\10.0.16299.0;C:\\Program Files (x86)\\Windows Kits\\10\\References\\10.0.16299.0")
(setenv "WindowsSdkBinPath" "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\")
(setenv "WindowsSdkDir" "C:\\Program Files (x86)\\Windows Kits\\10\\")
(setenv "WindowsSDKLibVersion" "10.0.16299.0\\")
(setenv "WindowsSdkVerBinPath" "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.16299.0\\")
(setenv "WindowsSDKVersion" "10.0.16299.0\\")
(setenv "WindowsSDK_ExecutablePath_x64" "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.6.1 Tools\\x64\\")
(setenv "WindowsSDK_ExecutablePath_x86" "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.6.1 Tools\\")
(setenv "__DOTNET_ADD_64BIT" "1")
(setenv "__DOTNET_PREFERRED_BITNESS" "64")
(setenv "__VSCMD_PREINIT_PATH" "C:\\Program Files\\ConEmu\\ConEmu\\Scripts;C:\\Program Files\\ConEmu;C:\\Program Files\\ConEmu\\ConEmu;C:\\Program Files (x86)\\Common Files\\Oracle\\Java\\javapath;C:\\Program Files\\ImageMagick-7.0.7-Q16;C:\\Python27;C:\\ProgramData\\Oracle\\Java\\javapath;C:\\Program Files (x86)\\Gpg4win\\..\\GnuPG\\bin;C:\\Program Files (x86)\\GnuWin32\\bin;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\bin;C:\\bin\\mpv;C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\Program Files\\MacType;C:\\bin\\emacs\\bin;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Program Files\\Everything;C:\\Users\\kadode\\.cargo\\bin;C:\\bin\\xperf\\;C:\\Program Files (x86)\\Windows Kits\\8.1\\Windows Performance Toolkit\\;C:\\bin\\arbtt\\bin;C:\\bin\\ruby\\bin;C:\\bin\\ffmpeg\\bin;C:\\Program Files\\Git\\cmd;C:\\Program Files (x86)\\GnuWin32\\lib;C:\\bin\\apache-ant-1.10.2\\bin;C:\\bin\\buck\\bin;C:\\Program Files\\Java\\jdk1.8.0_151\\bin;C:\\bin\\kotlinc\\bin;C:\\bin\\gradle-4.5.1\\bin;C:\\Program Files\\Cppcheck;C:\\Program Files\\Git\\bin;C:\\bin\\nc;C:\\Program Files\\Zulu\\zulu-7\\bin\\;C:\\Program Files\\Zulu\\zulu-8\\bin\\;C:\\Program Files\\nodejs\\;C:\\Program Files (x86)\\GtkSharp\\2.12\\bin;C:\\Program Files\\PuTTY\\;C:\\bin\\jflex-1.6.1\\bin;C:\\Program Files\\CMake\\bin;C:\\Program Files (x86)\\LLVM\\bin;C:\\Users\\kadode\\.cargo\\bin;C:\\Program Files (x86)\\mitmproxy\\bin;C:\\Users\\kadode\\AppData\\Roaming\\npm;C:\\Users\\kadode\\AppData\\Local\\Pandoc\\")

(provide 'vc)
;;; vc.el ends here
