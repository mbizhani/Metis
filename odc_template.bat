@echo off

set ODC_BASE_DIR=?

if exist "%UserProfile%\Documents\My Data Sources" (
	set ODC_BASE_DIR=%UserProfile%\Documents\My Data Sources
) else if exist "%UserProfile%\My Documents\My Data Sources" (
	set ODC_BASE_DIR=%UserProfile%\My Documents\My Data Sources
) else (
	echo ERROR: Can't find "My Data Sources" directory!
	pause
	goto :eof
)

echo ^<?xml version="1.0" encoding="utf-8" ?^> > "%ODC_BASE_DIR%\mydoc.odc"

for /f "useback delims=" %%_ in (%0) do (
  if "%%_"=="___ATAD___" set $=
  if defined $ echo(%%_ >> "%ODC_BASE_DIR%\mydoc.odc"
  if "%%_"=="___DATA___" set $=1
)
goto :eof

___DATA___
<html xmlns:o="urn:schemas-microsoft-com:office:office"
xmlns="http://www.w3.org/TR/REC-html40">

<head>
<meta http-equiv=Content-Type content="text/x-ms-odc; charset=utf-8">
<meta name=ProgId content=ODC.TableCollection>
<meta name=SourceType content=DATAFEED>

<xml id=docprops><o:DocumentProperties
  xmlns:o="urn:schemas-microsoft-com:office:office"
  xmlns="http://www.w3.org/TR/REC-html40">
  <o:Name>THE NAME IS HERE</o:Name>
 </o:DocumentProperties>
</xml>
<xml id=msodc><odc:OfficeDataConnection
  xmlns:odc="urn:schemas-microsoft-com:office:odc"
  xmlns="http://www.w3.org/TR/REC-html40">
  <odc:Connection odc:Type="DATAFEED">
   <odc:ConnectionString>Data Source=http://localhost:8080/ctx/CarService.svc/;Namespaces to Include=*;Max Received Message Size=4398046511104;Integrated Security=Basic;Keep Alive=true;Persist Security Info=false;Service Document Url=http://localhost:8080/ctx/CarService.svc/</odc:ConnectionString>
   <odc:CommandType>TableCollection</odc:CommandType>
   <odc:CommandText>&quot;Cars&quot;,&quot;Manufacturers&quot;</odc:CommandText>
  </odc:Connection>
 </odc:OfficeDataConnection>
</xml>
</html>
___ATAD___