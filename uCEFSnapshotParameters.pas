unit uCEFSnapshotParameters;

interface

uses SysUtils, uCEFTypes;

type
   TSnapshotOutputFormat = ( sofBMP, sofJPG );

   TSnapshotParameters = record
      ErrorText : String;        // if not empty, parsing ended up with errors
      URL : ustring;
      Width : Integer;
      Height : Integer;
      DelayMSec : Integer;
      OutputFormat : TSnapshotOutputFormat;
   end;

function ParseCommandLineParameters : TSnapshotParameters;

implementation

const
   cHelp = 'cefHtmlSnapshot [-arg1 value1] [-arg2 value2] ...'#10#10
         + '  -?, -h, -help    This inline documentation'#10
         + '  -url             URL of the website or file to be snapshotted (required)'#10
         + '  -width, -w       Width of the snapshot, between 1 and 2048 (default 1024)'#10
         + '  -height, -h      Height of the snapshot, between 1 and 2048 (default 768)'#10
         + '  -delay, -d       Delay in milliseconds, between 100 ms and 30 sec (default 1 sec)'#10
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

begin
   if ParamCount = 0 then begin
      Result.ErrorText := cHelp;
      Exit;
   end;

   Result.OutputFormat := sofBMP; // TODO
   Result.Width  := 1024;
   Result.Height := 768;
   Result.DelayMSec := 1000;

   var lastP := '';
   for var i := 1 to ParamCount do begin
      var p := ParamStr(i);
      if p = '' then continue;
      case p[1] of
         '-', '/' : begin
            lastP := LowerCase(Copy(p, 2));
         end;
      else
         if (lastP = '?') or (lastP = 'h') or (lastP = 'help') then begin
            Result.ErrorText := cHelp;
         end else if lastP = 'url' then begin
            Result.URL := p;
            // TODO: basic syntax check
         end else if (lastP = 'width') or (lastP = 'w') then begin
            Result.ErrorText := TryParseIntegerParameter('Width', p, Result.Width, 1, 2048);
         end else if (lastP = 'height') or (lastP = 'h') then begin
            Result.ErrorText := TryParseIntegerParameter('Height', p, Result.Height, 1, 2048);
         end else if (lastP = 'delay') or (lastP = 'd') then begin
            Result.ErrorText := TryParseIntegerParameter('Delay', p, Result.DelayMSec, 100, 30000);
         end else begin
            Result.ErrorText := 'Unsupported parameter "' + p + '"';
         end;
         lastP := '';
      end;
      if Result.ErrorText <> '' then Exit;
   end;
   if lastP <> '' then begin
      Result.ErrorText := 'Argument missing for parameter "' + lastP + '"';
   end else if Result.URL = '' then begin
      Result.ErrorText := 'Missing URL parameter, it is required';
   end;
end;

end.
