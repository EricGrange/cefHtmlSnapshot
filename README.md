# cefHtmlSnapshot 

Command-line utility for Windows whose aim is to take snapshots of HTML pages and save them as images or PDF

Started as hack of the "ConsoleBrowser2" demo of CEF4Delphi by Salvador Díaz Fau, additional bugs are mine.

Released under MPL 2.0

## Credits

Main repository at https://github.com/EricGrange/cefHtmlSnapshot

This project requires CEF4Dephi  https://github.com/salvadordf/CEF4Delphi/ by Salvador Díaz Fau
and leverages the Chromium Embedded Framework (https://bitbucket.org/chromiumembedded/cef/).

TurboJPEG support https://bitbucket.org/egrange/dwscript/src/master/Libraries/GraphicsLib/

## Command line arguments

```
cefHtmlSnapshot.exe url_or_file [-arg1 value1] [-arg2 value2] ... output_file

  -?, -h, --help    This inline documentation
  url_or_file       URL of the website or file to be snapshotted (required)
                    If a .url file is specified, the URL will be read from it
  output_file       Output file pathname, extension determines format (default snapshot.jpg)
                    Supported formats are pdf, jpg, png, bmp & txt

  -w, --width       Width of the snapshot, between 1 and 2048 (default 1024)
  -h, --height      Height of the snapshot, between 1 and 2048 (default 768)
                    When output format is a PDF, this parameter is ignored
  -d, --delay       Delay in milliseconds, between 100 ms and 30 sec (default 1 sec)
  -s, --scale       Scale of the website relative to 96dpi, between 0.1 and 10.0 (default 1.0)
  --quality         Output JPEG quality (1 to 100, default 90)
  --compression     Output PNG compresson level (0 to 9, default 7)

  --cookie          set a cookie, format is CommaText, starting with name=value, then fields
                    url=       cookie url (uses scheme + domain from url parameter by default)
                    domain=    cookie domain (uses domain from url parameter by default)
                    path=      cookie path ("/" by default)
                    secure=    secure flag (0 or 1, by default 1 if url starts with "https:")
                    httponly=  htpOnly flag (0 or 1, by default 0)
  --javascript      Name of a JavaScript file to execute just before taking the snapshot
  --ignore-certificate-errors Turns on/off certificate checks (0 or 1, by default 0)
  --no-sandbox      Turns on/off sandbox (0 or 1, by default 0)

  --pdf-xxx         PDF output options outlined below
        page-width      page width in microns (default 210000)
        page-height     page height in microns (default 297000)
        margins         sets all margins in points
        margin-top      top margin in points (default 20)
        margin-left     left margin in points (default 20)
        margin-right    right margin in points (default 20)
        margin-bottom   bottom margin in points (default 20)
        landscape       portait (default, 0) or landscape (1)
        backgrounds     enable backgrounds (1) or not (default, 0)
		
  --text            Specifies the source of text for .txt output, accepted values
                        text (default)
                        html
                        console
  --frame	        Specifies the name of the frame for text our html output (by default main frame)
		
```

## Exemple

Takes a 1024x768 snapshot of my blog and save it in Jpeg format, while running a JS script
to remove the EU cookie law banner.

```
cefHtmlSnapshot https://www.delphitools.info/ --javascript Scripts\cookie-law-buster.js snapshot.jpg
```
