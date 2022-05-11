unit uCEFSnapshotParameters;

interface

uses
   System.Classes, System.SysUtils, Vcl.Graphics, System.IniFiles,
   uCEFTypes, uCEFMiscFunctions;

const
   cChromiumSubFolder = 'Chromium101.0';
   cDLLSubfolder = 'Libraries';

type
   TSnapshotOutputFormat = (
      sofUnknown,    // format not et
      sofBMP,        // Windows BMP
      sofJPG,        // JPEG
      sofPNG,        // PNG
      sofPDF,        // PDF
      sofPrinter,    // sent to printer
      sofTXT         // Text, affected by TSnapshotTextSource
   );

   TSnapshotTextSource = (
      stsText,
      stsHTML,
      stsConsole
   );

   TSnapshotParameters = record
      ErrorText : String;        // if not empty, parsing ended up with errors
      URL : String;
      Width : Integer;
      Height : Integer;
      Scale : Double;
      DelayMSec : Integer;
      OutputFilePath : String;
      OutputFormat : TSnapshotOutputFormat;
      JPEGQuality : Integer;
      PNGCompressionLevel : Integer;
      PDFOptions : TCefPdfPrintSettings;
      PDFTitle, PDFURL : String;
      TextSource : TSnapshotTextSource;
      TextFrame : String;
      JavaScript : String;
      Cookies : array of String;
      IgnoreCertificateErrors : Boolean;
      NoSandbox : Boolean;
      Print : Boolean;

      procedure SaveBitmap(bmp : TBitmap);
      function SaveText(const txt : String) : Boolean;

      function URLSchemeDomain : String;
   end;

function ParseCommandLineParameters : TSnapshotParameters;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses LibTurboJPEG, Vcl.Imaging.pngimage, System.StrUtils, System.IOUtils;

// SaveText
//
      function SaveText(const txt : String) : Boolean;
begin

end;

const
   cHelp = 'cefHtmlSnaphot utility v0.7.100 - Html to image or pdf coversion using Chromium Embedded Framework'#10
         + 'Using CEF 100.0.23, CEF4Delphi, TurboJPEG see https://github.com/EricGrange/cefHtmlSnapshot'#10#10
         + 'cefHtmlSnapshot.exe url_or_file [-arg1 value1] [-arg2 value2] ... output_file'#10
         + #10
         + '  -?, -h, --help    This inline documentation'#10
         + '  url_or_file       URL of the website or file to be snapshotted (required)'#10
         + '                    If a .url file is specified, the URL will be read from it'#10
         + '  output_file       Output file pathname, extension determines format (default snapshot.jpg)'#10
         + '                    Supported formats are pdf, jpg, png, bmp, htm & txt.'#10
         + '                    If the printing mode is enabled, this is the name of the printer.'#10
         + #10
         + '  -w, --width       Width of the snapshot, between 1 and 2048 (default 1024)'#10
         + '  -h, --height      Height of the snapshot, between 1 and 2048 (default 768)'#10
         + '                    When output format is a PDF, this parameter is ignored'#10
         + '  -d, --delay       Delay in milliseconds, between 100 ms and 30 sec (default 1 sec)'#10
         + '  -s, --scale       Scale of the website relative to 96dpi, between 0.1 and 10.0 (default 1.0)'#10
         + '  --quality         Output JPEG quality (1 to 100, default 90)'#10
         + '  --compression     Output PNG compresson level (0 to 9, default 7)'#10
         + #10
         + '  --cookie          set a cookie, format is CommaText, starting with name=value, then fields'#10
         + '                    url=       cookie url (uses scheme + domain from url parameter by default)'#10
         + '                    domain=    cookie domain (uses domain from url parameter by default)'#10
         + '                    path=      cookie path ("/" by default)'#10
         + '                    secure=    secure flag (0 or 1, by default 1 if url starts with "https:")'#10
         + '                    httponly=  htpOnly flag (0 or 1, by default 0)'#10
         + '  --javascript      Name of a JavaScript file to execute just before taking the snapshot'#10
         + '  --ignore-certificate-errors Turns on/off certificate checks (0 or 1, by default 0)'#10
         + '  --no-sandbox      Turns on/off sandbox (0 or 1, by default 0)'#10
         + #10
         + '  --pdf-xxx         PDF output options outlined below'#10
         + '        page-width      page width in microns (default 210000)'#10
         + '        page-height     page height in microns (default 297000)'#10
         + '        margins         sets all margins in points'#10
         + '        margin-top      top margin in points (default 20)'#10
         + '        margin-left     left margin in points (default 20)'#10
         + '        margin-right    right margin in points (default 20)'#10
         + '        margin-bottom   bottom margin in points (default 20)'#10
         + '        landscape       portait (default, 0) or landscape (1)'#10
         + '        backgrounds     enable backgrounds (1) or not (default, 0)'#10
         + #10
         + '  --print           If this option 1 then output_file is the name of a printer (by default 0)'#10
         + #10
         + '  --text            Specifies the source of text for .txt output, accepted values'#10
         + '                        text (default)'#10
         + '                        html'#10
         + '                        console'#10
         + '  --frame           Specifies the name of the frame for text our html output (by default main frame)'#10
         ;

// ParseCommandLineParameters
//
function ParseCommandLineParameters : TSnapshotParameters;

   function TryParseIntegerParameter(const name, p : String; var value : Integer; mini, maxi : Integer) : String;
   begin
      value := StrToIntDef(p, mini-1);
      if (value < mini) or (value > maxi) then begin
         Result := 'Invalid ' + name + ' value: "' + p + '"';
      end else Result := '';
   end;

   function TryParseFloatParameter(const name, p : String; var value : Double; mini, maxi : Double) : String;
   begin
      value := StrToFloatDef(p, mini-1);
      if (value < mini) or (value > maxi) then begin
         Result := 'Invalid ' + name + ' value: "' + p + '"';
      end else Result := '';
   end;

   function URLFromURLFile(const fileName : String) : String;
   var
      ini : TIniFile;
   begin
      ini := TIniFile.Create(fileName);
      try
         Result := ini.ReadString('InternetShortcut', 'URL', '');
      finally
         ini.Free;
      end;
   end;

begin
   if ParamCount = 0 then begin
      Result.ErrorText := cHelp;
      Exit;
   end;

   Result.OutputFormat := sofJPG;  // JPEG by default
   Result.Width  := 1024;
   Result.Height := 768;
   Result.Scale := 1.0;
   Result.DelayMSec := 1000;
   Result.OutputFilePath := 'snapshot.bmp';
   Result.JPEGQuality := 90;
   Result.PNGCompressionLevel := 7;

   Result.PDFOptions.page_width := 210000;
   Result.PDFOptions.page_height := 297000;
   Result.PDFOptions.margin_type := PDF_PRINT_MARGIN_CUSTOM;
   Result.PDFOptions.margin_top := 20;
   Result.PDFOptions.margin_left := 20;
   Result.PDFOptions.margin_right := 20;
   Result.PDFOptions.margin_bottom := 20;
   Result.PDFOptions.landscape := 0;
   Result.PDFOptions.backgrounds_enabled := 0;

   if ParamCount < 2 then begin
      Result.ErrorText := cHelp;
      Exit;
   end;

   // first parameter is url_or_file

   Result.URL := ParamStr(1);
   if FileExists(Result.URL) then begin
      if SameText(ExtractFileExt(Result.URL), '.url') then begin
         Result.URL := URLFromURLFile(Result.URL);
         if Result.URL = '' then
            Result.ErrorText := 'URL not found in file';
      end else begin
         if CustomPathIsRelative(Result.URL) then
            Result.URL := IncludeTrailingPathDelimiter(GetCurrentDir) + Result.URL;
         Result.URL := 'file://' + Result.URL.Replace('\', '/');
      end;
   end;

   // last parameter is output_file

   Result.OutputFilePath := ParamStr(ParamCount);
   if CustomPathIsRelative(Result.OutputFilePath) then
      Result.OutputFilePath := IncludeTrailingPathDelimiter(GetCurrentDir) + Result.OutputFilePath;
   var ext := LowerCase(ExtractFileExt(Result.OutputFilePath));
   if ext = '.bmp' then
      Result.OutputFormat := sofBMP
   else if (ext = '.jpg') or (ext = '.jpeg') then
      Result.OutputFormat := sofJPG
   else if ext = '.png' then
      Result.OutputFormat := sofPNG
   else if ext = '.pdf' then
      Result.OutputFormat := sofPDF
   else if ext = '.txt' then
      Result.OutputFormat := sofTXT
   else if ext = '.htm' then begin
      Result.OutputFormat := sofTXT;
      Result.TextSource := stsHTML;
   end;

   // parse arguments in between

   var lastP := '';
   for var i := 2 to ParamCount-1 do begin
      var p := ParamStr(i);
      if p = '' then continue;
      case p[1] of
         '-', '/' : begin
            lastP := LowerCase(Copy(p, 2));
         end;
      else
         if (lastP = '?') or ((lastP = 'h') and (i = 2)) or (lastP = '-help') then begin
            Result.ErrorText := cHelp;
         end else if (lastP = '-width') or (lastP = 'w') then begin
            Result.ErrorText := TryParseIntegerParameter('Width', p, Result.Width, 1, 2048);
         end else if (lastP = '-height') or (lastP = 'h') then begin
            Result.ErrorText := TryParseIntegerParameter('Height', p, Result.Height, 1, 2048);
         end else if (lastP = '-scale') or (lastP = 's') then begin
            Result.ErrorText := TryParseFloatParameter('Scale', p, Result.Scale, 0.1, 10);
         end else if (lastP = '-delay') or (lastP = 'd') then begin
            Result.ErrorText := TryParseIntegerParameter('Delay', p, Result.DelayMSec, 100, 30000);
         end else if lastP = '-quality' then begin
            Result.ErrorText := TryParseIntegerParameter('Quality', p, Result.JPEGQuality, 1, 100);
         end else if lastP = '-compression' then begin
            Result.ErrorText := TryParseIntegerParameter('Quality', p, Result.JPEGQuality, 1, 100);
         end else if lastP = '-cookie' then begin
            var n := Length(Result.Cookies);
            SetLength(Result.Cookies, n+1);
            Result.Cookies[n] := p;
         end else if lastP = '-javascript' then begin
            if CustomPathIsRelative(p) then
               p := IncludeTrailingPathDelimiter(GetCurrentDir) + p;
            if not FileExists(p) then
               Result.ErrorText := 'Javascript file not found "' + p + '"';
            Result.JavaScript := p;
         end else if lastP = '-ignore-certificate-errors' then begin
            if p = '1' then
               Result.IgnoreCertificateErrors := True
            else if p <> '0' then
               Result.ErrorText := 'Unsupported option "' + p + '" for ignore-certificate-errors';
         end else if lastP = '-no-sandbox' then begin
            if p = '1' then
               Result.NoSandbox := True
            else if p <> '0' then
               Result.ErrorText := 'Unsupported option "' + p + '" for no-sandbox';
         end else if lastP = '-pdf-page-width' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-page-width', p, Result.PDFOptions.page_width, 10000, 10000000);
         end else if lastP = '-pdf-page-height' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-page-height', p, Result.PDFOptions.page_height, 10000, 10000000);
         end else if lastP = '-pdf-margins' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-margins', p, Result.PDFOptions.margin_top, 0, 10000);
            Result.PDFOptions.margin_left := Result.PDFOptions.margin_top;
            Result.PDFOptions.margin_right := Result.PDFOptions.margin_top;
            Result.PDFOptions.margin_bottom := Result.PDFOptions.margin_top;
         end else if lastP = '-pdf-margin-top' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-margin-top', p, Result.PDFOptions.margin_top, 0, 10000);
         end else if lastP = '-pdf-margin-left' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-margin-left', p, Result.PDFOptions.margin_left, 0, 10000);
         end else if lastP = '-pdf-margin-right' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-margin-right', p, Result.PDFOptions.margin_right, 0, 10000);
         end else if lastP = '-pdf-margin-bottom' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-margin-bottom', p, Result.PDFOptions.margin_bottom, 0, 10000);
         end else if lastP = '-pdf-landscape' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-landscape', p, Result.PDFOptions.landscape, 0, 1);
         end else if lastP = '-pdf-title' then begin
            Result.PDFTitle := p;      // undocumented, does not seem to work
         end else if lastP = '-pdf-url' then begin
            Result.PDFURL := p;        // undocumented, does not seem to work
         end else if lastP = '-pdf-backgrounds' then begin
            Result.ErrorText := TryParseIntegerParameter('PDF-backgrounds', p, Result.PDFOptions.backgrounds_enabled, 0, 1);
//      property scale_factor          : integer                 read Fscale_factor             write Fscale_factor          default 0;
//      property header_footer_enabled : boolean                 read Fheader_footer_enabled    write Fheader_footer_enabled default False;
//      property selection_only        : boolean                 read Fselection_only           write Fselection_only        default False;
         end else if lastP = '-print' then begin
            if p = '1' then begin
               Result.Print := True;
               Result.OutputFormat := sofPrinter;
            end else if p <> '0' then
               Result.ErrorText := 'Unsupported option "' + p + '" for print';
         end else if lastP = '-text' then begin
            if p = 'text' then
               Result.TextSource := stsText
            else if p = 'html' then
               Result.TextSource := stsHTML
            else if p = 'console' then
               Result.TextSource := stsConsole
            else Result.ErrorText := 'Unsupported option "' + p + '" for text';
         end else if lastP = '-frame' then begin
            Result.TextFrame := p;
         end else begin
            Result.ErrorText := 'Unsupported parameter "' + p + '"';
         end;
         lastP := '';
      end;
      if Result.ErrorText <> '' then Exit;
   end;

   if lastP <> '' then begin
      Result.ErrorText := 'Argument missing for parameter "' + lastP + '"';
   end else if Result.OutputFormat = sofUnknown then begin
      Result.ErrorText := 'Unsupported output file format "' + Result.OutputFilePath + '"';
   end else if (Result.URL = '') and not Result.Print then begin
      Result.ErrorText := 'Missing URL parameter, it is required';
   end;
end;

// SaveBitmapToJPEG
//
procedure SaveBitmapToJPEG(bmp : TBitmap; const fileName : String; quality : Integer);
begin
   LoadTurboJPEG(ExtractFilePath(ParamStr(0)) + cDLLSubfolder + '\turbojpeg-32.dll');
   var format := TJPF_UNKNOWN;
   case bmp.PixelFormat of
      pf32bit : format := TJPF_BGRA;
      pf24bit : format := TJPF_BGR;
   else
      Assert(False, 'Unsupported Bitmap PixelFormat');
   end;
   if format <> TJPF_UNKNOWN then begin
      var jpeg := TJ.InitCompress;
      try
         var outBuf : Pointer := nil;
         var outSize : Cardinal := 0;
         var pitch := 0;
         if bmp.Height > 1 then
            pitch := IntPtr(bmp.ScanLine[1]) - IntPtr(bmp.ScanLine[0]);
         if pitch >= 0 then begin
            if TJ.Compress2(jpeg, bmp.ScanLine[0], bmp.Width, pitch, bmp.Height, format,
                            @outBuf, @outSize, TJSAMP_420, quality, TJFLAG_PROGRESSIVE) <> 0 then
               RaiseLastTurboJPEGError(jpeg);
         end else begin
            if TJ.Compress2(jpeg, bmp.ScanLine[bmp.Height-1], bmp.Width, -pitch, bmp.Height, format,
                            @outBuf, @outSize, TJSAMP_420, quality, TJFLAG_PROGRESSIVE or TJFLAG_BOTTOMUP) <> 0 then
               RaiseLastTurboJPEGError(jpeg);
         end;
         try
            var fs := TFileStream.Create(fileName, fmCreate);
            try
               fs.Write(outBuf^, outSize);
            finally
               fs.Free;
            end;
         finally
           TJ.Free(outBuf);
         end;
      finally
         TJ.Destroy(jpeg);
      end;
      Exit;
   end;
end;

procedure SaveBitmapToPNG(bmp : TBitmap; const fileName : String; compressionLevel : Integer);
begin
   var png := TPNGImage.Create;
   try
      png.CompressionLevel := compressionLevel;
      png.Assign(bmp);
      png.SaveToFile(fileName);
   finally
      png.Free;
   end;
end;

// SaveBitmap
//
procedure TSnapshotParameters.SaveBitmap(bmp : TBitmap);
begin
   case OutputFormat of
      sofBMP : bmp.SaveToFile(OutputFilePath);
      sofJPG : SaveBitmapToJPEG(bmp, OutputFilePath, JPEGQuality);
      sofPNG : SaveBitmapToPNG(bmp, OutputFilePath, PNGCompressionLevel);
   end;
end;

// SaveText
//
function TSnapshotParameters.SaveText(const txt : String) : Boolean;
begin
   try
      TFile.WriteAllText(OutputFilePath, txt);
      Result := True;
   except
      Result := False;
   end;
end;

// URLSchemeDomain
//
function TSnapshotParameters.URLSchemeDomain : String;
begin
   var p := Pos('//', URL);
   Result := Copy(URL, 1, PosEx('/', URL, p+2));
end;

end.
