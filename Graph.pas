(*
  Component XYGraph, Version 3.0
  April 1999

  U.Jürß
  57078 Siegen, Germany
  e-mail: ujhs@aol.com

  Component XYGraph 3.0 is a versatile graph for showing 2D data:
   1. + Very flexible property design.
   2. + Extended control interface
        (control the graph and retrieve all curve data without writing one line
        of code by simply assigning TControl´s to the graph´s CONTROLS property).
   3. + Powerful, flickerfree CENTERZOOM (with or without aspect ratio).
   4. + Flickerfree REALPAN (single curves or graph).
   5. + Editing of curve controlpoints (move, insert, delete, freeze) by mouse
        or numerical input.
   6. + Free colored offset-lettering for every controlpoint.
   7. + Three markstyles with scaleable size to mark important controlpoints.
   8. + Relative cursor reading.
   9. + Streamed writing and reading single curves or graph.
  10. + DXF output of graph and curves for data exchange with CAD systems.
  11. + Moveable hintpanel for additional graph-information.
  12. + Example project showing some features.

  Properties:
    property Controls: TControls
        (all properties of type TControl can be TLabel, TStaticText, TStatusLabel
         or TPanel. They are used for data output. For example assigning a TLabel
         to the property "Mode" will display the graph´s actual mode.)
      property XOut: TControl (output) (normally the TEdit XIn)
      property YOut: TControl (output) (normally the TEdit YIn)
      property Mode: TControl (output)
      property Curve: TControl (output)
      property Item: TControl (output)
      property Color: TControl (output)
      property Angle: TControl (output)
        (TEdits are used for numerical inputs. (and outputs of X,Y))
      property XIn: TEdit (input)
      property YIn: TEdit (input)
        (TButtons are used as switches to control graph functions.)
      property Clear: TButton (input)
      property OpenView: TButton (input)
      property OpenPan: TButton (input)
      property Reset: TButton (input)
        (TRadioButtons are used to control the graph´s operating mode.)
      property ModeNone: TRadioButton (input)
      property ModeMove: TRadioButton (input)
      property ModeInsert: TRadioButton (input)
      property ModeDelete: TRadioButton (input)
      property ModeCursor: TRadioButton (input)
        (TCheckBoxes are used to control graph options.)
      property AspectRatio: TCheckBox (input)
      property MainGrid: TCheckBox (input)
      property SubGrid: TCheckBox (input)
      property HintPanel: TCheckBox (input)
      property ViewListBox: TCheckListBox (input)
        (ViewListBox is used to control visibility of curves. Click checkmark
         of the curve you want to show or hide. To show/hide the ViewListBox
         use Button "OpenView".)
      property PanListBox: TCheckListBox
        (PanListBox is used to select the curve(s) you want to move in the graph.
         To show/hide then PanListBox use Button "OpenPan".)

    property Colors: TColors
      property AxisBkGnd: color of xy-axis background.
      property TickColor: color of scaleticks.
      property GraphBkGnd: color of graph background.
      property MainGridColor: color of maingrid.
      property SubGridColor: color of subgrid.

    property Fonts: TFonts
      property AxisScale: font of axis-scale.
      property AxisTitle: font of axis title.
      property GraphTitle: font of graph title.

    property GraphTitle: Str32 string of graph title.

    property Positions: TPositions
      property XAxisLeft: left margin of xaxis.
      property XAxisRight: right margin of xaxis.
      property YAxisTop: top margin of yaxis.
      property YAxisBottom: bottom margin of yaxis.
      property TitleTop: top margin of graph title.
      property TitleLeft: left margin of graph title (centered if 0).
      property XAxisTitle: bottom margin of xaxis title.
      property YAxisTitle: left margin of yaxis title.

    property XAxis: TAxis
      property Title: title of axis
      property Min: min value of axis
      property Max: max value of axis
      property MainTicks: how many ticks with texture.
      property SubTicks: how many ticks between MainTicks.
      property MainTickLen: length of MainTicks.
      property SubTickLen: length of SubTicks.
      property Decimals: how many digits after comma.
      property ShowMainGrid: flag for showing MainGrid (for color look at Colors).
      property ShowSubGrid: flag for showing SubGrid (for color look at Colors).

    property YAxis: TAxis read FYAxis write FYAxis;
      (same as XAxis)

    property MaxZoom: defines the zoomlimits (MinZoom is 1 / MaxZoom).
    ----------------------------------------------------------------------------

    Control functions:
    LeftButton: does REALPAN if no other function is active. If one or
      more curves are selected in the PanListBox, then these curves are moved
      instead of panning the graph. You can also set this offset by numerical
      input via the X/Y TEdits.
      If a curve-controlpoint is active (shown by a marker), then
      the actual function (move, insert, delete) is performed.

    RightButton: does CENTERZOOM. The point you click will be centered
      and then zoomed by mouse movement. If you deselect "Aspect" the X and Y
      axis are zoomed independent.

    DoubleClick: Resets the graph (pan and zoom).

    Shift: if a curve-controlpoint is marked you can freeze this point by
      holding down the shift-key.
    ----------------------------------------------------------------------------

    How to do:
    Setup the graph to your needs with the properties.
    Use the
      function MakeCurve(AName: Str32; AColor: TColor; ALineWidth: Byte;
                         APenStyle: TPenStyle; AEnabled: Boolean);
    to create a new curve. The parameters makes the creation very flexible.
    Assuming AName = "test". If "test" already exists, it will be renamed to
    "test1" (just like Delphi does it with components).
    You get a handle to the created curve. With this handle you can add new
    points with the

      procedure AddPoint(AIndex: Integer; X,Y: TFloat);

    Where AIndex is the handle to the curve. After creation of all points you
    can add text and/or marks to every point you want with the

      procedure AddText(AIndex,APosition,AXOfs,AYOfs: Integer;
                       const AText: Str32; AColor: TColor);
      procedure AddMark(AIndex,APosition: Integer; AMarkType:
                        TMarkType; AColor: TColor);

    AIndex is the handle of the curve. APosition is the pointindex of the curve.
    AXOfs,AYOfs defines at witch offsets (relative to the point) where the text
    is displayed. Every text can be in different color. For every curve you can
    assign a font with the

      procedure SetCurveFont(AIndex: Integer; AName: TFontName;
                             ASize: Integer; AStyle: TFontStyles);

    For every curve you can assign the size of marks with the

      procedure SetMarkSize(AIndex: Integer; AMarkSize: TMarkSize);

    To add text to the HintPanel use Graph.HintPanel.Strings.Add('test');
    To clear the text use Graph.HintPanel.Strings.Clear;

    To use the edit functions of the graph, set Mode <> None. If moving the
    mouse cursor, a marker signs every controlpoint of the curve. Depending on
    the editmode you can move, delete or insert a point (only if the marker
    is visible). To freeze the actual marker, press (and hold down) the Shift-key.
    If EditMode is "Move", you can numerical input new point coordinates via
    the X,Y TEdits (if assigned).
    ----------------------------------------------------------------------------

    DXF-output: you can create a DXF file with the

      function MakeDXF(const FileName: string; FromX1,FromY1,FromX2,FromY2,
                       ToX1,ToY1,ToX2,ToY2,TextHeight: TFloat; Decimals: Byte): Boolean;

      FromX1..FromY2 are the source coordinates.
      ToX1..TY2 are the destination coordinates.
      Decimals is the precision after comma.
      All entities inside the source coordinates are transfered true to scale
      into the destination coordinates. Everything outside (text,scalelines etc.)
      are not true to scale. Does not process additional text (created with
      AddText) and marks (created with AddMark). 
    ----------------------------------------------------------------------------

    Interesting public methods are:

    function MakeCurve(const AName: Str32; AColor: TColor; ALineWidth: Byte;
                       APenStyle: TPenStyle; AEnabled: Boolean): Integer;
    procedure AddPoint(AIndex: Integer; X,Y: TFloat);
    procedure AddText(AIndex,APosition,AXOfs,AYOfs: Integer; const AText: Str32; AColor: TColor);
    procedure SetCurveFont(AIndex: Integer; AName: TFontName; ASize: Integer; AStyle: TFontStyles);
    procedure AddMark(AIndex,APosition: Integer; AMarkType: TMarkType; AColor: TColor);
    procedure SetMarkSize(AIndex: Integer; AMarkSize: TMarkSize);
    procedure ChangePoint(AIndex,APosition: Integer; X,Y: TFloat);
    procedure DeleteCurve(AItem: Integer);
    function GetCurveHandle(AName: Str32; var H: Integer): Boolean;
    function GetCurveName(H: Integer): Str32;
    procedure SetCurveEnabled(AIndex: Integer; Value: Boolean);
    procedure GetPoint(AIndex,APosition: Integer; var X,Y: TFloat);
    procedure InsertPoint(AIndex,APosition: Integer; X,Y: TFloat);
    procedure DeletePoint(AIndex,APosition: Integer);
    procedure Reset;
    procedure ShowHintPanel(Show: Boolean);
    procedure SetXOfs(AIndex: Integer; AOfs: TFloat);
    function GetXOfs(AIndex: Integer): TFloat;
    procedure SetYOfs(AIndex: Integer; AOfs: TFloat);
    function GetYOfs(AIndex: Integer): TFloat;
    procedure CheckCurvePoints(X,Y: Integer);
    procedure ChangeCPx(Fx: TFloat);
    procedure ChangeCPy(Fy: TFloat);
    procedure ChangeCurveOfs(Ox,Oy: TFloat; Relative: Boolean);
    procedure GetCPInfo(var CPMatch: Boolean; var CPCurve,CPIndex: Integer);
    procedure SetMode(Value: TMode);
    function MakeDXF(const FileName: string; FromX1,FromY1,FromX2,FromY2,
                     ToX1,ToY1,ToX2,ToY2,TextHeight: TFloat; Decimals: Byte): Boolean;
    function SaveCurveToFile(const FileName: string; Item: Integer): Boolean;
    function LoadCurveFromFile(const FileName: string): Boolean;
    function SaveGraphToFile(const FileName: string): Boolean;
    function LoadGraphFromFile(const FileName: string): Boolean;
    ----------------------------------------------------------------------------

    Look at the demo project to see some other features like writing and
    reading curves or graph, or creating DXF output.

    Excuse my english - I hope you get at least the gist of it.
*)

unit
  Graph;

interface

uses
  Windows,Classes,Controls,StdCtrls,ExtCtrls,Graphics,CheckLst;
{------------------------------------------------------------------------------}

const
  MaxHintLines = 10;
{------------------------------------------------------------------------------}

type
  TFloat = Double;
  Str32 = string[32];

  TMode = (gmNone,gmMove,gmInsert,gmDelete,gmCursor);
  TGraphStyleItems = (gsMainGrid,gsSubGrid,gsHintPanel);
  TGraphStyle = set of TGraphStyleItems;

  TMarkType = (mtBox,mtCircle,mtCross);
  TMarkSize = 2..8;

  TCurveData = record //Datenstruktur für SaveCurveToStream/LoadCurveFromStream
    Name: Str32;
    Enabled: Boolean;
    Color: TColor;
    LineWidth: Byte;
    PenStyle: TPenStyle;
    Points: Integer;
    Texts: Integer;
    Marks: Integer;
    XOfs: TFloat;
    YOfs: TFloat;
    FontName: Str32;
    FontSize: Integer;
    FontStyle: TFontStyles;
    MarkSize: TMarkSize;
  end;

  TGraphData = record //Datenstruktur für SaveGraphToFile/LoadGraphFromFile
    GraphTitle: Str32;
    Zoom: TFloat;
    MaxZoom: TFloat;
    Curves: Integer;
  end;

  PPointRec = ^TPointRec;
  TPointRec = record
             X: TFloat;
             Y: TFloat;
           end;

  PPointArray = ^TPointArray;
  TPointArray = array[0..0] of TPoint;

  PTextRec = ^TTextRec;
  TTextRec = record
    PointIndex: Integer;
    Text: Str32;
    TextColor: TColor;
    XOfs: Integer;
    YOfs: Integer;
  end;

  PMarkRec = ^TMarkRec;
  TMarkRec = record
    PointIndex: Integer;
    MarkType: TMarkType;
    MarkColor: TColor;
  end;

  TFontRec = record
    AxisScaleFontName: Str32;
    AxisScaleFontSize: Integer;
    AxisScaleFontStyle: TFontStyles;
    AxisTitleFontName: Str32;
    AxisTitleFontSize: Integer;
    AxisTitleFontStyle: TFontStyles;
    GraphTitleFontName: Str32;
    GraphTitleFontSize: Integer;
    GraphTitleFontStyle: TFontStyles;
  end;

  TDXFOut = class(TPersistent)
  private
    StringList: TStringList;
    FromXMin: TFloat;
    FromXMax: TFloat;
    FromYMin: TFloat;
    FromYMax: TFloat;
    ToXMin: TFloat;
    ToXMax: TFloat;
    ToYMin: TFloat;
    ToYMax: TFloat;
    TextHeight: TFloat;
    Decimals: Byte;
    LayerName: Str32;
  public
    constructor Create(AFromXMin,AFromYMin,AFromXMax,AFromYMax,AToXMin,AToYMin,
                       AToXMax,AToYMax,ATextHeight: TFloat; ADecimals: Byte);
    destructor Destroy; override;
    function FToA(F: TFloat): Str32;
    function ToX(X: TFloat): TFloat;
    function ToY(Y: TFloat): TFloat;
    procedure Header;
    procedure Trailer;
    procedure SetLayer(const Name: Str32);
    procedure Line(X1,Y1,Z1,X2,Y2,Z2: TFloat);
    procedure Point(X,Y,Z: TFloat);
    procedure StartPolyLine(Closed: Boolean);
    procedure Vertex(X,Y,Z: TFloat);
    procedure EndPolyLine;
    procedure DText(X,Y,Z,Height,Angle: TFloat; const Txt: Str32);
    procedure Layer;
    procedure StartPoint(X,Y,Z: TFloat);
    procedure EndPoint(X,Y,Z: TFloat);
    procedure AddText(const Txt: Str32);
  end;

  TXYGraph = class;

  TControls = class(TPersistent)
  private
    Graph: TXYGraph;
    FXOut: TControl;
    FYOut: TControl;
    FMode: TControl;
    FCurve: TControl;
    FItem: TControl;
    FColor: TControl;
    FAngle: TControl;
    FXIn: TEdit;
    FYIn: TEdit;
    FClear: TButton;
    FOpenView: TButton;
    FOpenPan: TButton;
    FReset: TButton;
    FModeNone: TRadioButton;
    FModeMove: TRadioButton;
    FModeInsert: TRadioButton;
    FModeDelete: TRadioButton;
    FModeCursor: TRadioButton;
    FAspectRatio: TCheckBox;
    FMainGrid: TCheckBox;
    FSubGrid: TCheckBox;
    FHintPanel: TCheckBox;
    FViewListBox: TCheckListBox;
    FPanListBox: TCheckListBox;
  protected
    procedure SetControl(Index: Integer; Value: TControl);
    procedure SetEdit(Index: Integer; Value: TEdit);
    procedure SetButton(Index: Integer; Value: TButton);
    procedure SetRadioButton(Index: Integer; Value: TRadioButton);
    procedure SetCheckBox(Index: Integer; Value: TCheckBox);
    procedure SetListBox(Index: Integer; Value: TCheckListBox);
  public
    constructor Create(AGraph: TXYGraph);
  published
    property XOut: TControl index 0 read FXOut write SetControl;
    property YOut: TControl index 1 read FYOut write SetControl;
    property Mode: TControl index 2 read FMode write SetControl;
    property Curve: TControl index 3 read FCurve write SetControl;
    property Item: TControl index 4 read FItem write SetControl;
    property Color: TControl index 5 read FColor write SetControl;
    property Angle: TControl index 6 read FAngle write SetControl;

    property XIn: TEdit index 0 read FXIn write SetEdit;
    property YIn: TEdit index 1 read FYIn write SetEdit;

    property Clear: TButton index 0 read FClear write SetButton;
    property OpenView: TButton index 1 read FOpenView write SetButton;
    property OpenPan: TButton index 2 read FOpenPan write SetButton;
    property Reset: TButton index 3 read FReset write SetButton;

    property ModeNone: TRadioButton index 0 read FModeNone write SetRadioButton;
    property ModeMove: TRadioButton index 1 read FModeMove write SetRadioButton;
    property ModeInsert: TRadioButton index 2 read FModeInsert write SetRadioButton;
    property ModeDelete: TRadioButton index 3 read FModeDelete write SetRadioButton;
    property ModeCursor: TRadioButton index 4 read FModeCursor write SetRadioButton;

    property AspectRatio: TCheckBox index 0 read FAspectRatio write SetCheckBox;
    property MainGrid: TCheckBox index 1 read FMainGrid write SetCheckBox;
    property SubGrid: TCheckBox index 2 read FSubGrid write SetCheckBox;
    property HintPanel: TCheckBox index 3 read FHintPanel write SetCheckBox;

    property ViewListBox: TCheckListBox index 0 read FViewListBox write SetListBox;
    property PanListBox: TCheckListBox index 1 read FPanListBox write SetListBox;
  end;

  THintPanel = class(TCustomPanel)
  private
    FStrings: TStringList;
    Graph: TXYGraph;
    Moving: Boolean;
    Start: Boolean;
    MouseX: Integer;
    MouseY: Integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure Loaded; override;
    procedure NewBounds;
    procedure DoStringsChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Strings: TStringList read FStrings write FStrings;
  end;

  TPositions = class(TPersistent)
  private
    FXAxisLeft: Integer;
    FXAxisRight: Integer;
    FYAxisTop: Integer;
    FYAxisBottom: Integer;
    FTitleTop: Integer;
    FTitleLeft: Integer;
    FXAxisTitle: Integer;
    FYAxisTitle: Integer;
    FOnChange: TNotifyEvent;
  protected
    procedure SetInteger(Index,Value: Integer);
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange default nil;
  published
    property XAxisLeft: Integer index 0 read FXAxisLeft write SetInteger;
    property XAxisRight: Integer index 1 read FXAxisRight write SetInteger;
    property YAxisTop: Integer index 2 read FYAxisTop write SetInteger;
    property YAxisBottom: Integer index 3 read FYAxisBottom write SetInteger;
    property TitleTop: Integer index 4 read FTitleTop write SetInteger;
    property TitleLeft: Integer index 5 read FTitleLeft write SetInteger;
    property XAxisTitle: Integer index 6 read FXAxisTitle write SetInteger;
    property YAxisTitle: Integer index 7 read FYAxisTitle write SetInteger;
  end;

  TFonts = class(TPersistent)
  private
    FAxisScale: TFont;
    FAxisTitle: TFont;
    FGraphTitle: TFont;
    FOnChange: TNotifyEvent;
  protected
    procedure SetFont(Index: Integer; Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AxisScale: TFont index 0 read FAxisScale write SetFont;
    property AxisTitle: TFont index 1 read FAxisTitle write SetFont;
    property GraphTitle: TFont index 2 read FGraphTitle write SetFont;
  end;

  TColors = class(TPersistent)
  private
    FAxisBkGnd: TColor;
    FTickColor: TColor;
    FGraphBkGnd: TColor;
    FMainGridColor: TColor;
    FSubGridColor: TColor;
    FOnChange: TNotifyEvent;
  protected
    procedure SetColor(Index: Integer; Value: TColor);
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AxisBkGnd: TColor index 0 read FAxisBkGnd write SetColor;
    property TickColor: TColor index 1 read FTickColor write SetColor;
    property GraphBkGnd: TColor index 2 read FGraphBkGnd write SetColor;
    property MainGridColor: TColor index 3 read FMainGridColor write SetColor;
    property SubGridColor: TColor index 4 read FSubGridColor write SetColor;
  end;

  TAxis = class(TPersistent)
  private
    FTitle: Str32;                                   {Beschriftung Achsentitel}
    FLength: Integer;                                              {Achsenlänge}
    FMin: TFloat;                                        {Anfangswert auf Achse}
    FMax: TFloat;                                            {Endwert auf Achse}
    FMinSave: TFloat;                                    {Anfangswert auf Achse}
    FMaxSave: TFloat;                                        {Endwert auf Achse}
    FZoom: TFloat;
    FMainTicks: Byte;                                    {Anzahl Hauptteilungen}
    FSubTicks: Byte;                                       {Anzahl Subteilungen}
    FMainTickLen: Byte;                           {Länge der Hauptskalenstriche}
    FSubTickLen: Byte;                              {Länge der Subskalenstriche}
    FDecimals: Byte;        {Anzahl Nachkommastellen für Beschriftung auf Achse}
    FShowMainGrid: Boolean;
    FShowSubGrid: Boolean;
    FFactor: TFloat;                          {Wertfaktor für 1 Pixel auf Achse}
    FValuePerMainTick: TFloat;
    FValuePerPixel: TFloat;
    FTicks: Integer;
    FPixelsPerSubTick: TFloat;
    FPan: Integer;
    FPanSubTicks: Integer;
    FOnChange: TNotifyEvent;
  protected
    procedure SetTitle(const Value: Str32);
    procedure SetLength(Value: Integer);
    procedure SetFloat(Index: Integer; Value: TFloat);
    procedure SetMax(Value: TFloat);
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetBoolean(Index: Integer; Value: Boolean);
  public
    constructor Create;
    procedure CalcAxis;
    function Value(APosition: Integer): TFloat;
    function Pixel(APosition: TFloat): Integer;
    procedure SetMinMax(AMin,AMax: TFloat);
    procedure SetZoom(Value: TFloat);
    procedure SetCenter(C: TFloat);
    procedure SetLeftBottom(L: TFloat);
    procedure SetRightTop(R: TFloat);
    function GetCenter: TFloat;
    property Length: Integer read FLength write SetLength default 200;
    property ValuePerMainTick: TFloat read FValuePerMainTick;
    property ValuePerPixel: TFloat read FValuePerPixel;
    property PixelsPerSubTick: TFloat read FPixelsPerSubTick;
    property Pan: Integer read FPan write FPan;
    property PanSubTicks: Integer read FPanSubTicks write FPanSubTicks;
    property Ticks: Integer read FTicks;
    property Zoom: TFloat read FZoom write FZoom;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Title: Str32 read FTitle write SetTitle;
    property Min: TFloat index 0 read FMin write SetFloat;
    property Max: TFloat index 1 read FMax write SetFloat;
    property MainTicks: Byte index 0 read FMainTicks write SetByte;
    property SubTicks: Byte index 1 read FSubTicks write SetByte;
    property MainTickLen: Byte index 2 read FMainTickLen write SetByte;
    property SubTickLen: Byte index 3 read FSubTickLen write SetByte;
    property Decimals: Byte index 4 read FDecimals write SetByte;
    property ShowMainGrid: Boolean index 0 read FShowMainGrid write SetBoolean;
    property ShowSubGrid: Boolean index 1 read FShowSubGrid write SetBoolean;
  end;

  TCurve = class(TPersistent)
  private
    FPoints: TList;
    FTexts: TList;
    FMarks: TList;
    FFont: TFont;
    FName: Str32;
    FEnabled: Boolean;
    FColor: TColor;
    FLineWidth: Byte;
    FPenStyle: TPenStyle;
    FXOfs: TFloat;
    FYOfs: TFloat;
    FMarkSize: TMarkSize;
    PPoint: PPointRec;
    PText: PTextRec;
    PMark: PMarkRec;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPoint(Ax,Ay: TFloat);
    procedure AddText(APointIndex,AXOfs,AYOfs: Integer; const AText: Str32; AColor: TColor);
    procedure AddMark(APointIndex: Integer; AMarkType: TMarkType; AColor: TColor);
    procedure GetPoint(AIndex: Integer; var Ax,Ay: TFloat);
    procedure ChangePoint(AIndex: Integer; Ax,Ay: TFloat);
    procedure InsertPoint(AIndex: Integer; Ax,Ay: TFloat);
    procedure DeletePoint(AIndex: Integer);
  public
    property Name: Str32 read FName write FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Color: TColor read FColor write FColor;
    property LineWidth: Byte read FLineWidth write FLineWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property XOfs: TFloat read FXOfs write FXOfs;
    property YOfs: TFloat read FYOfs write FYOfs;
    property MarkSize: TMarkSize read FMarkSize write FMarkSize;
  end;

  TXYGraph = class(TCustomPanel)
    procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer); override;
  private
    FXAxis: TAxis;
    FYAxis: TAxis;
    FColors: TColors;
    FPositions: TPositions;
    FFonts: TFonts;
    FCurve: TCurve;
    FCurveList: TList;
    FHintPanel: THintPanel;
    FControls: TControls;
    FMode: TMode;
    FGraphTitle: Str32;
    FZoom: TFloat;
    FMaxZoom: TFloat;

    DrawBmp: TBitMap;
    DXFOut: TDXFOut;
    MouseX: Integer;
    MouseY: Integer;
    CPBmp: TBitMap;
    CPRect: TRect;
    CPMatch: Boolean;                          {Flag für Kontrollpunkterkennung}
    CPCurve: Integer;                      {Index für Kurve des Kontrollpunktes}
    LastCPCurve: Integer;                  {Index für Kurve des Kontrollpunktes}
    CPIndex: Integer;                     {Index für Kontrollpunkt in der Kurve}
    LastCPIndex: Integer;                 {Index für Kontrollpunkt in der Kurve}
    CPx: TFloat;                                    {X-Wert des Kontrollpunktes}
    CPy: TFloat;                                    {Y-Wert des Kontrollpunktes}

    IsLoaded: Boolean;                                         {Flag für Loaded}
    BoundsChanged: Boolean;
    ZoomSave: TFloat;
    Freeze: Boolean;
    ZoomAspectRatio: Boolean;
    PanCurves: Boolean;
    HClip: HRgn;
  protected
    procedure Loaded; override;
    procedure DrawXAxis;
    procedure DrawYAxis;
    procedure OnChangePaint(Sender: TObject);
    procedure DoButtonClick(Sender: TObject);
    procedure DoRadioButtonClick(Sender: TObject);
    procedure DoCheckBoxClick(Sender: TObject);
    procedure DoListBoxClickCheck(Sender: TObject);
    procedure DoXEditExit(Sender: TObject);
    procedure DoYEditExit(Sender: TObject);

    procedure DoPan(Dx,Dy: Integer);
    procedure DoZoom(Dx,Dy: Integer);
    procedure DoMouse(X,Y: Integer);
    procedure SetMeasureCursor(X,Y: Integer);
    procedure DoMeasureCursor(X,Y: Integer);
    procedure DoMove(Dx,Dy: Integer);
    procedure DoCheckCP(X,Y: Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure DblClicked(Sender: TObject);
    procedure SetGraphTitle(const Value: Str32);
    procedure SetEditEnable(Value: Boolean);
    procedure OutMode(const Mode: Str32);
    procedure OutCurve(const Curve: Str32);
    procedure OutItem(Item: Integer);
    procedure OutColor(Color: TColor);
    procedure OutAngle(A: TFloat);
    procedure OutXY(Fx,Fy: TFloat);
    procedure ClearMarkBox;
    procedure DrawMarkBox;
    procedure DrawMark(ACanvas: TCanvas; MarkType: TMarkType;
                       MarkColor: TColor; MarkSize: TMarkSize; X,Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    function MakeCurve(const AName: Str32; AColor: TColor; ALineWidth: Byte;
                       APenStyle: TPenStyle; AEnabled: Boolean): Integer;
    procedure AddPoint(AIndex: Integer; X,Y: TFloat);
    procedure AddText(AIndex,APosition,AXOfs,AYOfs: Integer; const AText: Str32; AColor: TColor);
    procedure SetCurveFont(AIndex: Integer; AName: TFontName; ASize: Integer; AStyle: TFontStyles);
    procedure AddMark(AIndex,APosition: Integer; AMarkType: TMarkType; AColor: TColor);
    procedure SetMarkSize(AIndex: Integer; AMarkSize: TMarkSize);
    procedure ChangePoint(AIndex,APosition: Integer; X,Y: TFloat);
    procedure DeleteCurve(AItem: Integer);
    function GetCurveHandle(AName: Str32; var H: Integer): Boolean;
    function GetCurveName(H: Integer): Str32;
    procedure SetCurveEnabled(AIndex: Integer; Value: Boolean);
    procedure GetPoint(AIndex,APosition: Integer; var X,Y: TFloat);
    procedure InsertPoint(AIndex,APosition: Integer; X,Y: TFloat);
    procedure DeletePoint(AIndex,APosition: Integer);
    procedure Reset;
    procedure ShowHintPanel(Show: Boolean);
    procedure SetXOfs(AIndex: Integer; AOfs: TFloat);
    function GetXOfs(AIndex: Integer): TFloat;
    procedure SetYOfs(AIndex: Integer; AOfs: TFloat);
    function GetYOfs(AIndex: Integer): TFloat;
    procedure CheckCurvePoints(X,Y: Integer);
    procedure ChangeCPx(Fx: TFloat);   {X-Wert Kontrollpunkt Änderung von außen}
    procedure ChangeCPy(Fy: TFloat);   {Y-Wert Kontrollpunkt Änderung von außen}
    procedure ChangeCurveOfs(Ox,Oy: TFloat; Relative: Boolean);
    procedure GetCPInfo(var CPMatch: Boolean; var CPCurve,CPIndex: Integer);
    procedure SetZoom(Value: TFloat);
    procedure SetMode(Value: TMode);
    function GetMaxPoints: Integer;
    function XAxisPixel(Value: TFloat): Integer;
    function YAxisPixel(Value: TFloat): Integer;

    function MakeDXF(const FileName: string; FromX1,FromY1,FromX2,FromY2,
                     ToX1,ToY1,ToX2,ToY2,TextHeight: TFloat; Decimals: Byte): Boolean;
    procedure DXFAxis;
    procedure DXFCurves;

    function SaveCurveToStream(FileStream: TFileStream; Item: Integer): Boolean;
    function LoadCurveFromStream(FileStream: TFileStream): Boolean;
    function SaveCurveToFile(const FileName: string; Item: Integer): Boolean;
    function LoadCurveFromFile(const FileName: string): Boolean;
    function SaveGraphToFile(const FileName: string): Boolean;
    function LoadGraphFromFile(const FileName: string): Boolean;

    property HintPanel: THintPanel read FHintPanel write FHintPanel;
    property Mode: TMode read FMode write SetMode;
    property Zoom: TFloat read FZoom write SetZoom;
  published
    property Align;
    //property Anchors;
    property Hint;
    property ShowHint;
    property OnClick;
    property OnKeyPress;
    property OnKeyDown;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;

    property Colors: TColors read FColors write FColors;
    property Fonts: TFonts read FFonts write FFonts;
    property GraphTitle: Str32 read FGraphTitle write SetGraphTitle;
    property Positions: TPositions read FPositions write FPositions;
    property XAxis: TAxis read FXAxis write FXAxis;
    property YAxis: TAxis read FYAxis write FYAxis;
    property Controls: TControls read FControls write FControls;
    property MaxZoom: TFloat read FMaxZoom write FMaxZoom;
  end;
{------------------------------------------------------------------------------}

procedure Register;
function AtoF(S: Str32; var F: TFloat): Boolean;
function InRange(Test,Min,Max: Integer): Boolean;
function Angle(X1,Y1,X2,Y2: TFloat): TFloat;       {0°-360° gegen Uhrzeigersinn}
procedure TextOutRotate(ACanvas: TCanvas; X,Y: Integer; Ang: Word; S: Str32);

implementation
{------------------------------------------------------------------------------}

uses
  SysUtils,Forms,Dialogs,ComCtrls;
{------------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Udo',[TXYGraph]);
end;
{------------------------------------------------------------------------------}

function AtoF(S: Str32; var F: TFloat): Boolean;
var
  Code: Integer;
begin
  Code:=Pos(',',S);
  if Code > 0 then S[Code]:='.';
  Val(S,F,Code);
  Result:=Code = 0;
end;
{------------------------------------------------------------------------------}

function InRange(Test,Min,Max: Integer): Boolean;
begin
  Result:=(Test >= Min) and (Test <= Max);
end;
{------------------------------------------------------------------------------}

function Angle(X1,Y1,X2,Y2: TFloat): TFloat;                 {Steigung = 0..90°}
var                                                          {Gefälle = 0..-90°}
  Dx: TFloat;
  Dy: TFloat;
begin
  Result:=0;
  Dx:=X2 - X1;
  Dy:=Y2 - Y1;
  if Dx <> 0 then Result:=ArcTan(Dy / Dx) / Pi * 180;
  if Dx < 0 then Result:=-Result;
end;
{------------------------------------------------------------------------------}

procedure TextOutRotate(ACanvas: TCanvas; X,Y: Integer; Ang: Word; S: Str32);
var
  LogRec: TLogFont;
  OldFontHandle: HFont;
  NewFontHandle: HFont;
begin
  GetObject(ACanvas.Font.Handle,SizeOf(LogRec),@LogRec);
  LogRec.lfEscapement:=Ang;
  NewFontHandle:=CreateFontIndirect(LogRec);
  OldFontHandle:=SelectObject(ACanvas.Handle,NewFontHandle);
  ACanvas.TextOut(X,Y,S);
  NewFontHandle:=SelectObject(ACanvas.Handle,OldFontHandle);
  DeleteObject(NewFontHandle);
end;
{------------------------------------------------------------------------------}

constructor TCurve.Create;
begin
  inherited Create;
  FPoints:=TList.Create;
  FTexts:=TList.Create;
  FMarks:=TList.Create;
  FFont:=TFont.Create;
  FFont.Name:='small font';
  FFont.Size:=7;
  FFont.Style:=[];
  FMarkSize:=4;
  FEnabled:=True;
  FColor:=clWhite;
  FLineWidth:=1;
  FPenStyle:=psSolid;
  FXOfs:=0.0;
  FYOfs:=0.0;
end;
{------------------------------------------------------------------------------}

destructor TCurve.Destroy;
var
  I: Integer;
begin
  for I:=0 to Pred(FPoints.Count) do FreeMem(FPoints.Items[I],SizeOf(TPointRec));
  FPoints.Free;
  for I:=0 to Pred(FTexts.Count) do FreeMem(FTexts.Items[I],SizeOf(TTextRec));
  FTexts.Free;
  for I:=0 to Pred(FMarks.Count) do FreeMem(FMarks.Items[I],SizeOf(TMarkRec));
  FMarks.Free;
  FFont.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}

procedure TCurve.AddPoint(Ax,Ay: TFloat);
begin
  GetMem(PPoint,SizeOf(TPointRec));
  PPoint^.X:=Ax;
  PPoint^.Y:=Ay;
  FPoints.Add(PPoint);
end;
{------------------------------------------------------------------------------}

procedure TCurve.AddText(APointIndex,AXOfs,AYOfs: Integer; const AText: Str32; AColor: TColor);
begin
  GetMem(PText,SizeOf(TTextRec));
  PText^.PointIndex:=APointIndex;
  PText^.XOfs:=AXOfs;
  PText^.YOfs:=AYOfs;
  PText^.Text:=AText;
  PText^.TextColor:=AColor;
  FTexts.Add(PText);
end;
{------------------------------------------------------------------------------}

procedure TCurve.AddMark(APointIndex: Integer; AMarkType: TMarkType; AColor: TColor);
begin
  GetMem(PMark,SizeOf(TMarkRec));
  PMark^.PointIndex:=APointIndex;
  PMark^.MarkType:=AMarkType;
  PMark^.MarkColor:=AColor;
  FMarks.Add(PMark);
end;
{------------------------------------------------------------------------------}

procedure TCurve.GetPoint(AIndex: Integer; var Ax,Ay: TFloat);
begin
  if InRange(AIndex,0,Pred(FPoints.Count)) then
  begin
    PPoint:=FPoints.Items[AIndex];
    Ax:=PPoint^.X + FXOfs;
    Ay:=PPoint^.Y + FYOfs;
  end;
end;
{------------------------------------------------------------------------------}

procedure TCurve.ChangePoint(AIndex: Integer; Ax,Ay: TFloat);
begin
  if InRange(AIndex,0,Pred(FPoints.Count)) then
  begin
    PPoint:=FPoints.Items[AIndex];
    PPoint^.X:=Ax - FXOfs;
    PPoint^.Y:=Ay - FYOfs;
  end;
end;
{------------------------------------------------------------------------------}

procedure TCurve.InsertPoint(AIndex: Integer; Ax,Ay: TFloat);
begin
  if AIndex > -1 then
  begin
    GetMem(PPoint,SizeOf(TPointRec));
    PPoint^.X:=Ax;
    PPoint^.Y:=Ay;
    FPoints.Insert(AIndex,PPoint);
  end;
end;
{------------------------------------------------------------------------------}

procedure TCurve.DeletePoint(AIndex: Integer);
begin
  if InRange(AIndex,0,Pred(FPoints.Count)) then
  begin
    FreeMem(FPoints.Items[AIndex],SizeOf(TPointRec));
    FPoints.Delete(AIndex);
  end;
end;
{------------------------------------------------------------------------------}

constructor TPositions.Create;
begin
  inherited Create;
  FXAxisLeft:=60;
  FXAxisRight:=15;
  FYAxisTop:=30;
  FYAxisBottom:=50;
  FTitleTop:=5;
  FTitleLeft:=0;
  FXAxisTitle:=20;
  FYAxisTitle:=5;
  FOnChange:=nil;
end;
{------------------------------------------------------------------------------}

procedure TPositions.SetInteger(Index,Value: Integer);
begin
  case Index of
    0 : FXAxisLeft:=Value;
    1 : FXAxisRight:=Value;
    2 : FYAxisTop:=Value;
    3 : FYAxisBottom:=Value;
    4 : FTitleTop:=Value;
    5 : FTitleLeft:=Value;
    6 : FXAxisTitle:=Value;
    7 : FYAxisTitle:=Value;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{------------------------------------------------------------------------------}

constructor TFonts.Create;
begin
  inherited Create;
  FAxisScale:=TFont.Create;
  FAxisScale.Name:='small fonts';
  FAxisScale.Size:=7;
  FAxisScale.Color:=clNavy;

  FAxisTitle:=TFont.Create;
  FAxisTitle.Name:='arial';
  FAxisTitle.Size:=8;
  FAxisTitle.Style:=[fsBold];
  FAxisTitle.Color:=clMaroon;

  FGraphTitle:=TFont.Create;
  FGraphTitle.Name:='arial';
  FGraphTitle.Size:=10;
  FGraphTitle.Style:=[fsBold];
  FGraphTitle.Color:=clMaroon;
  FOnChange:=nil;
end;
{------------------------------------------------------------------------------}

destructor TFonts.Destroy;
begin
  FAxisScale.Free;
  FAxisTitle.Free;
  FGraphTitle.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}

procedure TFonts.SetFont(Index: Integer; Value: TFont);
begin
  case Index of
    0 : FAxisScale.Assign(Value);
    1 : FAxisTitle.Assign(Value);
    2 : FGraphTitle.Assign(Value);
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{------------------------------------------------------------------------------}

constructor TColors.Create;
begin
  inherited Create;
  FAxisBkGnd:=clSilver;
  FTickColor:=clBlack;
  FGraphBkGnd:=clBlack;
  FMainGridColor:=clGray;
  FSubGridColor:=clGray;
  FOnChange:=nil;
end;
{------------------------------------------------------------------------------}

procedure TColors.SetColor(Index: Integer; Value: TColor);
begin
  case Index of
    0: FAxisBkGnd:=Value;
    1: FTickColor:=Value;
    2: FGraphBkGnd:=Value;
    3: FMainGridColor:=Value;
    4: FSubGridColor:=Value;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{------------------------------------------------------------------------------}

constructor TAxis.Create;
begin
  inherited Create;
  FTitle:='Axis-Title';
  FLength:=200;
  FMin:=0;
  FMax:=10;
  FMinSave:=FMin;
  FMaxSave:=FMax;
  FZoom:=1.0;
  FMainTicks:=5;
  FSubTicks:=5;
  FMainTickLen:=10;
  FSubTickLen:=5;
  FDecimals:=2;
  FPan:=0;
  FPanSubTicks:=0;
  FShowMainGrid:=True;
  FShowSubGrid:=False;
  FOnChange:=nil;
  CalcAxis;
end;
{------------------------------------------------------------------------------}

procedure TAxis.CalcAxis;
begin
  FValuePerMainTick:=(FMax - FMin) / FMainTicks;
  FFactor:=FLength / (FMax - FMin);
  FTicks:=FMainTicks * FSubTicks;
  FPixelsPerSubTick:=FLength / FTicks;
  FValuePerPixel:=FValuePerMainTick / (FSubTicks * FPixelsPerSubTick);
end;
{------------------------------------------------------------------------------}

function TAxis.Value(APosition: Integer): TFloat;
begin
  Result:=FMin + (FValuePerPixel * (APosition - FPan));
end;
{------------------------------------------------------------------------------}

function TAxis.Pixel(APosition: TFloat): Integer;
begin
  Result:=FPan + Round((APosition - FMin) * FFactor);
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetZoom(Value: TFloat);
var
  Zoom,Dif: TFloat;
begin
  Dif:=FMax - FMin;
  Zoom:=Dif / (Value * FZoom);
  FMin:=FMin - Dif + Zoom;
  FMax:=FMax + Dif - Zoom;
  CalcAxis;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetCenter(C: TFloat);
var
  Dif: TFloat;
begin
  Dif:=(FMax - FMin) / 2;
  FMin:=C - Dif;
  FMax:=C + Dif;
  CalcAxis;
end;
{------------------------------------------------------------------------------}

function TAxis.GetCenter: TFloat;
begin
  Result:=FMin + ((FMax - FMin) / 2);
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetLeftBottom(L: TFloat);
var
  Dif: TFloat;
begin
  Dif:=FMax - FMin;
  FMin:=L;
  FMax:=FMin + Dif;
  CalcAxis;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetRightTop(R: TFloat);
var
  Dif: TFloat;
begin
  Dif:=FMax - FMin;
  FMax:=R;
  FMin:=FMax - Dif;
  CalcAxis;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetMinMax(AMin,AMax: TFloat);
begin
  if (AMin < FMax) and (AMax > FMin) then
  begin
    FMin:=AMin;
    FMax:=AMax;
  end;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetTitle(const Value: Str32);
begin
  if FTitle <> Value then
  begin
    FTitle:=Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetLength(Value: Integer);
begin
  if FLength <> Value then
  begin
    FLength:=Value;
    CalcAxis;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetFloat(Index: Integer; Value: TFloat);
begin
  case Index of
    0: if (Value <> FMin) and (Value < FMax) then
       begin
         FMin:=Value;
         FMinSave:=FMin;
         CalcAxis;
         if Assigned(FOnChange) then FOnChange(Self);
       end;
    1: if (Value <> FMax) and (Value > FMin) then
       begin
         FMax:=Value;
         FMaxSave:=Value;
         CalcAxis;
         if Assigned(FOnChange) then FOnChange(Self);
       end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetMax(Value: TFloat);
begin
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetByte(Index: Integer; Value: Byte);
begin
  case Index of
    0 : if Value > 0 then FMainTicks:=Value;
    1 : FSubTicks:=Value;
    2 : FMainTickLen:=Value;
    3 : FSubTickLen:=Value;
    4 : if Value < 5 then FDecimals:=Value;
  end;
  CalcAxis;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{------------------------------------------------------------------------------}

procedure TAxis.SetBoolean(Index: Integer; Value: Boolean);
begin
  case Index of
    0 : FShowMainGrid:=Value;
    1 : FShowSubGrid:=Value;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;
{------------------------------------------------------------------------------}

constructor TXYGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IsLoaded:=False;
  BoundsChanged:=False;
  ZoomAspectRatio:=True;
  PanCurves:=False;
  CPMatch:=False;
  FZoom:=1.0;
  MaxZoom:=5.0;
  SetBounds(Left,Top,400,300);

  FXAxis:=TAxis.Create;
  FYAxis:=TAxis.Create;
  FColors:=TColors.Create;
  FFonts:=TFonts.Create;
  FPositions:=TPositions.Create;
  FCurveList:=TList.Create;
  DrawBmp:=TBitMap.Create;

  FControls:=TControls.Create(Self);

  FHintPanel:=THintPanel.Create(Self);
  FHintPanel.Parent:=Self;
  FHintPanel.Visible:=True;

  FXAxis.OnChange:=OnChangePaint;
  FYAxis.OnChange:=OnChangePaint;
  FFonts.OnChange:=OnChangePaint;
  FColors.OnChange:=OnChangePaint;
  FPositions.OnChange:=OnChangePaint;

  OnDblClick:=DblClicked;

  FGraphTitle:='Graph-Title';
  XAxis.Title:='X-Axis-Title';
  YAxis.Title:='Y-Axis-Title';
end;
{------------------------------------------------------------------------------}

destructor TXYGraph.Destroy;
var
  I: Integer;
begin
  FXAxis.Free;
  FYAxis.Free;
  FPositions.Free;
  FColors.Free;
  FFonts.Free;
  for I:=0 to Pred(FCurveList.Count) do
  begin
    FCurve:=FCurveList.Items[I];
    FCurve.Free;
  end;
  FCurveList.Free;
  DrawBmp.Free;
  FHintPanel.Free;
  FControls.Free;

  inherited Destroy;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.Loaded;
begin
  inherited Loaded;
  XAxis.FMinSave:=XAxis.FMin;
  XAxis.FMaxSave:=XAxis.FMax;
  YAxis.FMinSave:=YAxis.FMin;
  YAxis.FMaxSave:=YAxis.FMax;

  XAxis.CalcAxis;
  YAxis.CalcAxis;

  ZoomSave:=Zoom;

  SetMode(gmNone);
  IsLoaded:=True;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
begin
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
  BoundsChanged:=True;
end;
{------------------------------------------------------------------------------}

function TXYGraph.XAxisPixel(Value: TFloat): Integer;
begin
  Result:=Positions.XAxisLeft + XAxis.Pixel(Value);
end;
{------------------------------------------------------------------------------}

function TXYGraph.YAxisPixel(Value: TFloat): Integer;
begin
  Result:=Height - Positions.YAxisBottom - YAxis.Pixel(Value);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.Paint;
var
  R: TRect;
  H,I,J: Integer;
  X,Y: TFloat;
  Size: Word;
  PA: PPointArray;
  PText: PTextRec;
  PMark: PMarkRec;
begin
  if BoundsChanged then
  begin
    XAxis.Length:=Width - Positions.XAxisLeft - Positions.XAxisRight;
    YAxis.Length:=Height - Positions.YAxisTop - Positions.YAxisBottom;
    BoundsChanged:=False;
  end;

  if CPMatch or (FMode = gmCursor) then ClearMarkBox;

  DrawBmp.Width:=Width;
  DrawBmp.Height:=Height;
  DrawBmp.Canvas.Pen.Width:=1;

  DrawBmp.Canvas.Brush.Color:=FColors.FGraphBkGnd;
  DrawBmp.Canvas.FillRect(Rect(FPositions.XAxisLeft,FPositions.YAxisTop,
                     Width - Positions.XAxisRight,Height - Positions.YAxisBottom));
  DrawBmp.Canvas.Brush.Color:=FColors.AxisBkGnd;
  DrawBmp.Canvas.FillRect(Rect(0,0,Positions.XAxisLeft,Height));
  DrawBmp.Canvas.FillRect(Rect(Positions.XAxisLeft,0,Width,Positions.YAxisTop));
  DrawBmp.Canvas.FillRect(Rect(0,Height - Positions.YAxisBottom,Width,Height));
  DrawBmp.Canvas.FillRect(Rect(Width - Positions.XAxisRight,Positions.YAxisTop,Width,Height - FPositions.YAxisBottom));

  DrawBmp.Canvas.Brush.Color:=clGray;
  R:=Rect(0,0,Width,Height);
  DrawBmp.Canvas.FrameRect(R);
  InflateRect(R,-1,-1);
  DrawBmp.Canvas.Brush.Color:=clWhite;
  DrawBmp.Canvas.FrameRect(R);
  DrawBmp.Canvas.Brush.Style:=bsClear;

  if Length(FGraphTitle) > 0 then
  begin
    DrawBmp.Canvas.Font:=FFonts.FGraphTitle;
    if Positions.TitleLeft = 0 then
      DrawBmp.Canvas.TextOut(Width div 2 - DrawBmp.Canvas.TextWidth(FGraphTitle) div 2,
                        Positions.TitleTop,FGraphTitle)
    else DrawBmp.Canvas.TextOut(Positions.TitleLeft,Positions.TitleTop,FGraphTitle);
  end;

  DrawXAxis;
  DrawYAxis;

  HClip:=CreateRectRgn(Positions.XAxisLeft,Positions.YAxisTop,
                       Width - Positions.XAxisRight + 1,Height - Positions.YAxisBottom + 1);
  SelectClipRgn(DrawBmp.Canvas.Handle,HClip);
  Size:=GetMaxPoints * SizeOf(TPointArray);
  GetMem(PA,Size);

  for H:=0 to Pred(FCurveList.Count) do
  begin
    FCurve:=FCurveList.Items[H];
    if FCurve.Enabled and (FCurve.FPoints.Count > 0) then
    begin
      DrawBmp.Canvas.Pen.Color:=FCurve.Color;
      DrawBmp.Canvas.Pen.Style:=FCurve.PenStyle;
      DrawBmp.Canvas.Pen.Width:=FCurve.LineWidth;
      J:=Pred(FCurve.FPoints.Count);
      for I:=0 to J do
      begin
        FCurve.GetPoint(I,X,Y);
        PA^[I].x:=XAxisPixel(X);
        PA^[I].y:=YAxisPixel(Y);
      end;
      DrawBmp.Canvas.PolyLine(Slice(PA^,Succ(J)));
      for I:=0 to Pred(FCurve.FTexts.Count) do
      begin
        PText:=FCurve.FTexts.Items[I];
        DrawBmp.Canvas.Font:=FCurve.FFont;
        DrawBmp.Canvas.Font.Color:=PText^.TextColor;
        DrawBmp.Canvas.Brush.Style:=bsClear;
        FCurve.GetPoint(PText^.PointIndex,X,Y);
        DrawBmp.Canvas.TextOut(XAxisPixel(X) + PText^.XOfs,
                               YAxisPixel(Y) + PText^.YOfs,PText^.Text);
      end;
      for I:=0 to Pred(FCurve.FMarks.Count) do
      begin
        PMark:=FCurve.FMarks.Items[I];
        FCurve.GetPoint(PMark^.PointIndex,X,Y);
        DrawMark(DrawBmp.Canvas,PMark^.MarkType,PMark^.MarkColor,
                 FCurve.FMarkSize,XAxisPixel(X),YAxisPixel(Y));
      end;
    end;
  end;
  FreeMem(PA,Size);
  DeleteObject(HClip);

  DrawBmp.Canvas.Pen.Style:=psSolid;
  R:=ClientRect;
  Canvas.CopyRect(R,DrawBmp.Canvas,R);

  if CPMatch or (FMode = gmCursor) then DrawMarkBox;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OnChangePaint(Sender: TObject);
begin
  if Sender = FPositions then
  begin
    XAxis.Length:=Width - FPositions.XAxisLeft - FPositions.XAxisRight;
    YAxis.Length:=Height - FPositions.YAxisTop - FPositions.YAxisBottom;
  end;
  Application.ProcessMessages;
  if IsLoaded then Paint;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DrawXAxis;
var
  I,X,Y: Integer;
  Pos: Integer;
  VPos: Integer;
  S: string[12];
begin
  DrawBmp.Canvas.Font:=FFonts.AxisScale;
  DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
  Y:=0;
  VPos:=Height - FPositions.YAxisBottom;
  FXAxis.PanSubTicks:=Round(FXAxis.Pan / FXAxis.PixelsPerSubTick);
  if FXAxis.Pan > 0 then
  begin
    if FXAxis.PanSubTicks >= FXAxis.SubTicks then
    begin
      FXAxis.Pan:=FXAxis.Pan - Round(FXAxis.PanSubTicks * FXAxis.PixelsPerSubTick);
      FXAxis.PanSubTicks:=0;
      FXAxis.SetMinMax(FXAxis.Min - FXAxis.ValuePerMainTick,
                       FXAxis.Max - FXAxis.ValuePerMainTick);
    end;
    for X:=-FXAxis.PanSubTicks to FXAxis.Ticks - FXAxis.PanSubTicks do
    begin
      Pos:=FPositions.XAxisLeft + Round(FXAxis.FPixelsPerSubTick * X) + FXAxis.Pan;
      if (Pos >= FPositions.XAxisLeft) and (Pos <= Width - FPositions.XAxisRight) then
      begin
        DrawBmp.Canvas.MoveTo(Pos,VPos);
        if X mod FXAxis.SubTicks = 0 then
        begin
          DrawBmp.Canvas.LineTo(Pos,VPos + FXAxis.MainTickLen);
          S:=FloatToStrF(FXAxis.Min + (Y * FXAxis.ValuePerMainTick),ffFixed,7,FXAxis.Decimals);
          for I:=1 to Length(S) do
          if S[I] = ' ' then Delete(S,I,1);
          I:=DrawBmp.Canvas.TextWidth(S);
          DrawBmp.Canvas.TextOut(Pos - I div 2,VPos + FXAxis.MainTickLen,S);
          if FXAxis.ShowMainGrid then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.MainGridColor;
            DrawBmp.Canvas.MoveTo(Pos,FPositions.YAxisTop);
            DrawBmp.Canvas.LineTo(Pos,Height - FPositions.YAxisBottom);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end
        else
        begin
          DrawBmp.Canvas.LineTo(Pos,VPos + FXAxis.SubTickLen);
          if FXAxis.ShowSubGrid then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.SubGridColor;
            DrawBmp.Canvas.MoveTo(Pos,FPositions.YAxisTop);
            DrawBmp.Canvas.LineTo(Pos,Height - FPositions.YAxisBottom);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end;
      end;
      if X mod FXAxis.SubTicks = 0 then Inc(Y);
    end;
  end
  else
  begin
    if FXAxis.PanSubTicks <= -FXAxis.SubTicks then
    begin
      FXAxis.Pan:=FXAxis.Pan - Round(FXAxis.PanSubTicks * FXAxis.PixelsPerSubTick);
      FXAxis.PanSubTicks:=0;
      FXAxis.SetMinMax(FXAxis.Min + FXAxis.ValuePerMainTick,
                       FXAxis.Max + FXAxis.ValuePerMainTick);
    end;
    for X:=FXAxis.Ticks - FXAxis.PanSubTicks downto 0 - FXAxis.PanSubTicks do
    begin
      Pos:=FPositions.XAxisLeft + Round(FXAxis.FPixelsPerSubTick * X) + FXAxis.Pan;
      if (Pos >= FPositions.XAxisLeft) and (Pos <= Width - FPositions.XAxisRight) then
      begin
        DrawBmp.Canvas.MoveTo(Pos,VPos);
        if X mod FXAxis.SubTicks = 0 then
        begin
          DrawBmp.Canvas.LineTo(Pos,VPos + FXAxis.MainTickLen);
          S:=FloatToStrF(FXAxis.Max - (Y * FXAxis.ValuePerMainTick),ffFixed,7,FXAxis.Decimals);
          for I:=1 to Length(S) do if S[I] = ' ' then Delete(S,I,1);
          I:=DrawBmp.Canvas.TextWidth(S);
          DrawBmp.Canvas.TextOut(Pos - I div 2,VPos + FXAxis.MainTickLen,S);
          if FXAxis.ShowMainGrid then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.MainGridColor;
            DrawBmp.Canvas.MoveTo(Pos,FPositions.YAxisTop);
            DrawBmp.Canvas.LineTo(Pos,Height - FPositions.YAxisBottom);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end
        else
        begin
          DrawBmp.Canvas.LineTo(Pos,VPos + FXAxis.SubTickLen);
          if FXAxis.ShowSubGrid then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.SubGridColor;
            DrawBmp.Canvas.MoveTo(Pos,FPositions.YAxisTop);
            DrawBmp.Canvas.LineTo(Pos,Height - FPositions.YAxisBottom);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end;
      end;
      if X mod FXAxis.SubTicks = 0 then Inc(Y);
    end;
  end;
  DrawBmp.Canvas.Pen.Color:=FColors.GraphBkGnd;
  DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,VPos);
  DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,VPos);
  DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,FPositions.YAxisTop);
  DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,FPositions.YAxisTop);

  I:=DrawBmp.Canvas.TextWidth(XAxis.Title);
  X:=FPositions.XAxisLeft + XAxis.Length div 2 - I div 2;
  DrawBmp.Canvas.Font:=FFonts.AxisTitle;
  DrawBmp.Canvas.TextOut(X,Height - FPositions.FXAxisTitle,XAxis.Title);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DrawYAxis;
var
  I,H,X,Y: Integer;
  Pos: Integer;
  S: string[12];
begin
  DrawBmp.Canvas.Font:=FFonts.AxisScale;
  DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
  H:=DrawBmp.Canvas.TextHeight('0');
  Y:=0;
  FYAxis.PanSubTicks:=Round(FYAxis.Pan / FYAxis.PixelsPerSubTick);
  if FYAxis.Pan > 0 then
  begin
    if FYAxis.PanSubTicks >= FYAxis.SubTicks then
    begin
      FYAxis.Pan:=FYAxis.Pan - Round(FYAxis.PanSubTicks * FYAxis.PixelsPerSubTick);
      FYAxis.PanSubTicks:=0;
      FYAxis.SetMinMax(FYAxis.Min - FYAxis.ValuePerMainTick,
                       FYAxis.Max - FYAxis.ValuePerMainTick);
    end;
    for X:=-FYAxis.PanSubTicks to FYAxis.Ticks - FYAxis.PanSubTicks do
    begin
      Pos:=Height - FPositions.YAxisBottom - Round(FYAxis.FPixelsPerSubTick * X) - FYAxis.Pan;
      if (Pos >= FPositions.YAxisTop) and (Pos <= Height - FPositions.YAxisBottom) then
      begin
        DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Pos);
        if X mod FYAxis.SubTicks = 0 then
        begin
          DrawBmp.Canvas.LineTo(FPositions.XAxisLeft - FYAxis.MainTickLen,Pos);
          S:=FloatToStrF(FYAxis.Min + (Y * FYAxis.ValuePerMainTick),ffFixed,7,FYAxis.Decimals);
          for I:=1 to Length(S) do
          if S[I] = ' ' then Delete(S,I,1);
          I:=DrawBmp.Canvas.TextWidth(S);
          DrawBmp.Canvas.TextOut(FPositions.XAxisLeft - FYAxis.MainTickLen - I,Pos - H div 2,S);
          if FYAxis.ShowMainGrid and (Pos <> FPositions.YAxisTop)
                                and (Pos <> Height - FPositions.YAxisBottom) then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.MainGridColor;
            DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Pos);
            DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,Pos);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end
        else
        begin
          DrawBmp.Canvas.LineTo(FPositions.XAxisLeft - FYAxis.SubTickLen,Pos);
          if FYAxis.ShowSubGrid then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.SubGridColor;
            DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Pos);
            DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,Pos);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end;
      end;
      if X mod FYAxis.SubTicks = 0 then Inc(Y);
    end;
  end
  else
  begin
    if FYAxis.PanSubTicks <= -FYAxis.SubTicks then
    begin
      FYAxis.Pan:=FYAxis.Pan - Round(FYAxis.PanSubTicks * FYAxis.PixelsPerSubTick);
      FYAxis.PanSubTicks:=0;
      FYAxis.SetMinMax(FYAxis.Min + FYAxis.ValuePerMainTick,
                       FYAxis.Max + FYAxis.ValuePerMainTick);
    end;
    for X:=FYAxis.Ticks - FYAxis.PanSubTicks downto 0 - FYAxis.PanSubTicks do
    begin
      Pos:=Height - FPositions.YAxisBottom - Round(FYAxis.FPixelsPerSubTick * X) - FYAxis.Pan;
      if (Pos >= FPositions.YAxisTop) and (Pos <= Height - FPositions.YAxisBottom) then
      begin
        DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Pos);
        if X mod FYAxis.SubTicks = 0 then
        begin
          DrawBmp.Canvas.LineTo(FPositions.XAxisLeft - FYAxis.MainTickLen,Pos);
          S:=FloatToStrF(FYAxis.Max - (Y * FYAxis.ValuePerMainTick),ffFixed,7,FYAxis.Decimals);
          for I:=1 to Length(S) do
          if S[I] = ' ' then Delete(S,I,1);
          I:=DrawBmp.Canvas.TextWidth(S);
          DrawBmp.Canvas.TextOut(FPositions.XAxisLeft - FYAxis.MainTickLen - I,Pos - H div 2,S);
          if FYAxis.ShowMainGrid and (Pos <> FPositions.YAxisTop)
                                and (Pos <> Height - FPositions.YAxisBottom) then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.MainGridColor;
            DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Pos);
            DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,Pos);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end
        else
        begin
          DrawBmp.Canvas.LineTo(FPositions.XAxisLeft - FYAxis.SubTickLen,Pos);
          if FYAxis.ShowSubGrid then
          begin
            DrawBmp.Canvas.Pen.Color:=FColors.SubGridColor;
            DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Pos);
            DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,Pos);
            DrawBmp.Canvas.Pen.Color:=FColors.FTickColor;
          end;
        end;
      end;
      if X mod FYAxis.SubTicks = 0 then Inc(Y);
    end;
  end;
  DrawBmp.Canvas.Pen.Color:=FColors.GraphBkGnd;
  DrawBmp.Canvas.MoveTo(FPositions.XAxisLeft,Height - FPositions.YAxisBottom);
  DrawBmp.Canvas.LineTo(FPositions.XAxisLeft,FPositions.YAxisTop - 1);
  DrawBmp.Canvas.MoveTo(Width - FPositions.XAxisRight,Height - FPositions.YAxisBottom);
  DrawBmp.Canvas.LineTo(Width - FPositions.XAxisRight,FPositions.YAxisTop - 1);

  I:=DrawBmp.Canvas.TextWidth(YAxis.Title);
  Y:=Height - FPositions.YAxisBottom - YAxis.Length div 2 + I div 2;
  DrawBmp.Canvas.Font:=FFonts.AxisTitle;
  TextOutRotate(DrawBmp.Canvas,FPositions.FYAxisTitle,Y,900,YAxis.Title);
end;
{------------------------------------------------------------------------------}

function TXYGraph.GetMaxPoints: Integer;
var
  I,Max: Integer;
begin
  Max:=0;
  for I:=0 to Pred(FCurveList.Count) do
  begin
    FCurve:=FCurveList.Items[I];
    if FCurve.FPoints.Count > Max then Max:=FCurve.FPoints.Count;
  end;
  Result:=Max;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetEditEnable(Value: Boolean);
begin
  if Assigned(FControls.XOut) then FControls.XOut.Enabled:=Value;
  if Assigned(FControls.YOut) then FControls.YOut.Enabled:=Value;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OutMode(const Mode: Str32);
begin
  if Assigned(FControls.FMode) then
  begin
    if (FControls.FMode is TPanel) then TPanel(FControls.FMode).Caption:=Mode;
    if (TObject(FControls.FMode) is TStatusPanel) then TStatusPanel(FControls.FMode).Text:=Mode;
    if (FControls.FMode is TLabel) then TLabel(FControls.FMode).Caption:=Mode;
    if (FControls.FMode is TStaticText) then TStaticText(FControls.FMode).Caption:=Mode;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OutCurve(const Curve: Str32);
begin
  if Assigned(FControls.FCurve) then
  begin
    if (FControls.FCurve is TPanel) then TPanel(FControls.FCurve).Caption:=Curve;
    if (TObject(FControls.FCurve) is TStatusPanel) then TStatusPanel(FControls.FCurve).Text:=Curve;
    if (FControls.FCurve is TLabel) then TLabel(FControls.FCurve).Caption:=Curve;
    if (FControls.FCurve is TStaticText) then TStaticText(FControls.FCurve).Caption:=Curve;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OutItem(Item: Integer);
var
  Text: Str32;
begin
  if Item > -1 then Text:=IntToStr(Item) else Text:='';
  if Assigned(FControls.FItem) then
  begin
    if (FControls.FItem is TPanel) then TPanel(FControls.FItem).Caption:=Text;
    if (TObject(FControls.FItem) is TStatusPanel) then TStatusPanel(FControls.FItem).Text:=Text;
    if (FControls.FItem is TLabel) then TLabel(FControls.FItem).Caption:=Text;
    if (FControls.FItem is TStaticText) then TStaticText(FControls.FItem).Caption:=Text;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OutColor(Color: TColor);
begin
  if Assigned(FControls.FColor) then
  begin
    if (FControls.FColor is TPanel) then TPanel(FControls.FColor).Color:=Color;
    if (FControls.FColor is TLabel) then TLabel(FControls.FColor).Color:=Color;
    if (FControls.FColor is TStaticText) then TStaticText(FControls.FColor).Color:=Color;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OutXY(Fx,Fy: TFloat);
var
  Sx,Sy: Str32;
begin
  Sx:=FloatToStrF(Fx,ffFixed,7,3);
  Sy:=FloatToStrF(Fy,ffFixed,7,3);

  if Assigned(FControls.FXOut) then
  begin
    if (FControls.FXOut is TEdit) then TEdit(FControls.FXOut).Text:=Sx;
    if (FControls.FXOut is TPanel) then TPanel(FControls.FXOut).Caption:=Sx;
    if (TObject(FControls.FXOut) is TStatusPanel) then TStatusPanel(FControls.FXOut).Text:=Sx;
    if (FControls.FXOut is TLabel) then TLabel(FControls.FXOut).Caption:=Sx;
    if (FControls.FXOut is TStaticText) then TStaticText(FControls.FXOut).Caption:=Sx;
  end;

  if Assigned(FControls.FYOut) then
  begin
    if (FControls.FYOut is TEdit) then TEdit(FControls.FYOut).Text:=Sy;
    if (FControls.FYOut is TPanel) then TPanel(FControls.FYOut).Caption:=Sy;
    if (TObject(FControls.FYOut) is TStatusPanel) then TStatusPanel(FControls.FYOut).Text:=Sy;
    if (FControls.FYOut is TLabel) then TLabel(FControls.FYOut).Caption:=Sy;
    if (FControls.FYOut is TStaticText) then TStaticText(FControls.FYOut).Caption:=Sy;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.OutAngle(A: TFloat);
var
  Sa: Str32;
begin
  if Assigned(FControls.FAngle) then
  begin
    if A > 9.9E16 then Sa:='' else Sa:=FloatToStrF(A,ffFixed,7,3);
    if (FControls.FAngle is TPanel) then TPanel(FControls.FAngle).Caption:=Sa;
    if (TObject(FControls.FAngle) is TStatusPanel) then TStatusPanel(FControls.FAngle).Text:=Sa;
    if (FControls.FAngle is TLabel) then TLabel(FControls.FAngle).Caption:=Sa;
    if (FControls.FAngle is TStaticText) then TStaticText(FControls.FAngle).Caption:=Sa;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoPan(Dx,Dy: Integer);
begin
  {if not PanCurves then
  begin
    OutMode('Pan: Graph');
    SetEditEnable(False);
    if Dx <> 0 then FXAxis.Pan:=FXAxis.Pan + Dx;
    if Dy <> 0 then FYAxis.Pan:=FYAxis.Pan - Dy;
    Paint;
  end
  else
  begin
    OutMode('Pan: Curves');
    SetEditEnable(True);
    ChangeCurveOfs(Dx * FXAxis.FValuePerPixel,-Dy * FYAxis.FValuePerPixel,True);
  end; }
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoZoom(Dx,Dy: Integer);
var
  AXDif,AYDif: TFloat;
  XDif,YDif: TFloat;
  ZFx,ZFy: TFloat;
  Factor: TFloat;
  XStep,YStep: TFloat;
begin
  AXDif:=FXAxis.FMax - FXAxis.FMin;
  AYDif:=FYAxis.FMax - FYAxis.FMin;
  XDif:=FXAxis.FMaxSave - FXAxis.FMinSave;
  YDif:=FYAxis.FMaxSave - FYAxis.FMinSave;
  ZFx:=1.0;
  ZFy:=1.0;
  if AXDif <> 0.0 then ZFx:=XDif / AXDif;
  if AYDif <> 0.0 then ZFy:=YDif / AYDif;

  XStep:=0.0;
  YStep:=0.0;

  if Dx > 0 then XStep:=-0.02 else if Dx < 0 then XStep:=0.02;
  if Dy > 0 then YStep:=-0.02 else if Dy < 0 then YStep:=0.02;

  if ZoomAspectRatio then
  begin
    OutMode('Zoom: Aspect');
    OutXY(ZFx,ZFx);
    Factor:=FXAxis.Zoom + XStep;
    if ((Factor < 1) and (ZFx < FMaxZoom)) or
       ((Factor > 1) and (ZFx > 1 / FMaxZoom)) then
    begin
      FXAxis.SetZoom(Factor);
      FYAxis.SetZoom(Factor);
      Paint;
    end;
  end
  else 
  begin
    OutMode('Zoom: Free');
    OutXY(ZFx,ZFy);
    Factor:=FXAxis.Zoom + XStep;
    if ((Factor < 1) and (ZFx < FMaxZoom)) or
       ((Factor > 1) and (ZFx > 1 / FMaxZoom)) then FXAxis.SetZoom(Factor);
    Factor:=FXAxis.Zoom - YStep;
    if ((Factor < 1) and (ZFy < FMaxZoom)) or
       ((Factor > 1) and (ZFy > 1 / FMaxZoom)) then FYAxis.SetZoom(Factor);
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoMouse(X,Y: Integer);
begin
  if FMode = gmCursor then OutMode('Mouse: Cursor')
    else OutMode('Mouse: Position');
  OutXY(XAxis.Value(X - Positions.XAxisLeft),
        YAxis.Value(Height - Y - Positions.YAxisBottom));
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetMeasureCursor(X,Y: Integer);
begin
  ClearMarkBox;
  OutMode('Mouse: Cursor');
  OutXY(0,0);
  ClearMarkBox;
  CPMatch:=False;
  CPx:=XAxis.Value(X - Positions.XAxisLeft);
  CPy:=YAxis.Value(Height - Y - Positions.YAxisBottom);
  DrawMarkBox;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoMeasureCursor(X,Y: Integer);
begin
  OutXY(XAxis.Value(X - Positions.XAxisLeft) - CPx,
        YAxis.Value(Height - Y - Positions.YAxisBottom) - CPy);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoMove(Dx,Dy: Integer);
begin
  if CPMatch then
  begin
    CPx:=CPx + Dx * FXAxis.ValuePerPixel;
    CPy:=CPy - Dy * FYAxis.ValuePerPixel;
    ChangePoint(CPCurve,CPIndex,CPx,CPy);
    OutXY(CPx,CPy);
    OutMode('Edit: Move');
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoCheckCP(X,Y: Integer);
var
  Fx,Fy: TFloat;
begin
  CheckCurvePoints(X - Positions.XAxisLeft,Height - Y - Positions.YAxisBottom);
  if CPMatch then
  begin
    if ((LastCPCurve <> CPCurve) or (LastCPIndex <> CPIndex)) then
    begin
      OutXY(CPx,CPy);
      FCurve:=FCurveList.Items[CPCurve];
      OutCurve(FCurve.Name);
      OutItem(CPIndex);
      OutColor(FCurve.Color);
      LastCPCurve:=CPCurve;
      LastCPIndex:=CPIndex;
      if CPIndex < Pred(FCurve.FPoints.Count) then
      begin
        GetPoint(CPCurve,Succ(CPIndex),Fx,Fy);
        if Assigned(FControls.FAngle) then OutAngle(Angle(CPx,CPy,Fx,Fy));
      end;
    end;
  end
  else if not CPMatch then
  begin
    OutCurve('');
    OutItem(-1);
    OutColor(FColors.FGraphBkGnd);
    OutAngle(9.9E18);
    LastCPCurve:=-1;
    LastCPIndex:=-1;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DblClicked(Sender: TObject);
begin
 // Reset;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Cx,Cy,Dx,Dy,Fx,Fy: TFloat;
  N: Integer;
begin
  inherited MouseDown(Button,Shift,X,Y);

  MouseX:=X;
  MouseY:=Y;

  if Button = mbLeft then
  begin
    if not CPMatch then DoPan(0,0) else DoMouse(X,Y);
    case FMode of
      gmMove: DoMove(0,0);
      gmInsert: if CPMatch then
                begin
                  GetPoint(CPCurve,CPIndex,CPx,CPy);
                  CPx:=XAxis.Value(X - Positions.XAxisLeft);
                  CPy:=YAxis.Value(Height - Y - Positions.YAxisBottom);
                  InsertPoint(CPCurve,CPIndex,CPx,CPy);
                  Paint;
                end;
      gmDelete: if CPMatch then
                begin
                  DeletePoint(CPCurve,CPIndex);
                  Paint;
                end;
      gmCursor: SetMeasureCursor(X,Y);
    end;
  end
  else if Button = mbRight then
  {begin
    Cx:=FXAxis.GetCenter;
    Cy:=FYAxis.GetCenter;
    Dx:=FXAxis.Value(X - Positions.XAxisLeft);
    Dy:=FYAxis.Value(Height - Y - Positions.YAxisBottom);
    if Cx > Dx then Fx:=(Cx - Dx) / 10 else Fx:=(Dx - Cx) / 10;
    if Cy > Dy then Fy:=(Cy - Dy) / 10 else Fy:=(Dy - Cy) / 10;
    for N:=1 to 10 do
    begin
      if Cx > Dx then FXAxis.SetCenter(Cx - N * Fx)
        else FXAxis.SetCenter(Cx + N * Fx);
      if Cy > Dy then FYAxis.SetCenter(Cy - N * Fy)
        else FYAxis.SetCenter(Cy + N * Fy);
      Paint;
    end;
    DoZoom(0,0);
  end;}
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  Dx,Dy: Integer;
begin
  inherited MouseMove(Shift,X,Y);

  Dx:=X - MouseX;
  Dy:=Y - MouseY;

  Freeze:=ssShift in Shift;

  if ssLeft in Shift then
  begin
    if not CPMatch then DoPan(Dx,Dy);
    case FMode of
      gmMove: DoMove(Dx,Dy);
      gmCursor: DoMeasureCursor(X,Y);
    end;
  end
  else if ssRight in Shift then DoZoom(Dx,Dy)
  else
  begin
    if (FMode <> gmNone) and (FMode <> gmCursor) then DoCheckCP(X,Y);
    if FMode = gmCursor then DoMeasureCursor(X,Y)
    else if (FMode = gmNone) and (LastCPIndex > -1) then DoCheckCP(MaxInt,MaxInt)
    else if not CPMatch then DoMouse(X,Y);
  end;

  if not PanCurves and (FMode = gmMove) then SetEditEnable(CPMatch)
    else if PanCurves then SetEditEnable(True);

  MouseX:=X;
  MouseY:=Y;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetGraphTitle(const Value: Str32);
begin
  if Value <> FGraphTitle then
  begin
    FGraphTitle:=Value;
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.Reset;
begin
  FXAxis.FZoom:=1.0;
  FYAxis.FZoom:=1.0;
  FXAxis.FPan:=0;
  FYAxis.FPan:=0;
  FXAxis.FMin:=FXAxis.FMinSave;
  FXAxis.FMax:=FXAxis.FMaxSave;
  FYAxis.FMin:=FYAxis.FMinSave;
  FYAxis.FMax:=FYAxis.FMaxSave;
  FXAxis.CalcAxis;
  FYAxis.CalcAxis;
  Zoom:=ZoomSave;
  Paint;
end;
{------------------------------------------------------------------------------}

function TXYGraph.MakeCurve(const AName: Str32; AColor: TColor; ALineWidth: Byte;
                            APenStyle: TPenStyle; AEnabled: Boolean): Integer;
var
  H,N: Integer;
  S: Str32;
begin
  N:=0;
  S:=AName;
  while GetCurveHandle(S,H) do
  begin
    Inc(N);
    S:=AName + IntToStr(N);
  end;
  FCurve:=TCurve.Create;
  FCurve.Name:=S;
  FCurve.Color:=AColor;
  FCurve.LineWidth:=ALineWidth;
  FCurve.PenStyle:=APenStyle;
  FCurve.Enabled:=AEnabled;
  FCurveList.Add(FCurve);
  Result:=FCurveList.IndexOf(FCurve);
  if Assigned(FControls.FViewListBox) then
  begin
    FControls.FViewListBox.Items.Add(S);
    FControls.FViewListBox.Checked[Pred(FControls.FViewListBox.Items.Count)]:=AEnabled;
  end;
  if Assigned(FControls.FPanListBox) then
  begin
    FControls.FPanListBox.Items.Add(S);
    FControls.FPanListBox.Checked[Pred(FControls.FPanListBox.Items.Count)]:=False;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DeleteCurve(AItem: Integer);
begin
  if AItem < FCurveList.Count then
  begin
    FCurve:=FCurveList.Items[AItem];
    FCurveList.Delete(AItem);
    FCurve.Destroy;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.GetCurveHandle(AName: Str32; var H: Integer): Boolean;
var
  I,J: Integer;
begin
  H:=-1;
  J:=FCurveList.Count;
  I:=0;
  AName:=AnsiUpperCase(AName);
  while I < J do
  begin
    FCurve:=FCurveList.Items[I];
    if AnsiUpperCase(FCurve.Name) = AName then
    begin
      H:=I;
      Break;
    end;
    Inc(I);
  end;
  Result:=I < J;
end;
{------------------------------------------------------------------------------}

function TXYGraph.GetCurveName(H: Integer): Str32;
begin
  Result:='';
  if (H < 0) or (H > Pred(FCurveList.Count)) then Exit;
  FCurve:=FCurveList.Items[H];
  Result:=FCurve.Name;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.ChangePoint(AIndex,APosition: Integer; X,Y: TFloat);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    if APosition < FCurve.FPoints.Count then FCurve.ChangePoint(APosition,X,Y);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.GetPoint(AIndex,APosition: Integer; var X,Y: TFloat);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    if InRange(APosition,0,Pred(FCurve.FPoints.Count)) then
      FCurve.GetPoint(APosition,X,Y);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.AddPoint(AIndex: Integer; X,Y: TFloat);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.AddPoint(X,Y);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.AddMark(AIndex,APosition: Integer; AMarkType: TMarkType; AColor: TColor);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.AddMark(APosition,AMarkType,AColor);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetMarkSize(AIndex: Integer; AMarkSize: TMarkSize);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.FMarkSize:=AMarkSize;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.AddText(AIndex,APosition,AXOfs,AYOfs: Integer; const AText: Str32; AColor: TColor);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.AddText(APosition,AXOfs,AYOfs,AText,AColor);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetCurveFont(AIndex: Integer; AName: TFontName; ASize: Integer; AStyle: TFontStyles);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.FFont.Name:=AName;
    FCurve.FFont.Size:=ASize;
    FCurve.FFont.Style:=AStyle;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.InsertPoint(AIndex,APosition: Integer; X,Y: TFloat);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.InsertPoint(APosition,X,Y);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DeletePoint(AIndex,APosition: Integer);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.DeletePoint(APosition);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetXOfs(AIndex: Integer; AOfs: TFloat);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.XOfs:=AOfs;
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.GetXOfs(AIndex: Integer): TFloat;
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    Result:=FCurve.XOfs;
  end
  else Result:=0;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetYOfs(AIndex: Integer; AOfs: TFloat);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.YOfs:=AOfs;
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.GetYOfs(AIndex: Integer): TFloat;
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    Result:=FCurve.YOfs;
  end
  else Result:=0;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetCurveEnabled(AIndex: Integer; Value: Boolean);
begin
  if InRange(AIndex,0,Pred(FCurveList.Count)) then
  begin
    FCurve:=FCurveList.Items[AIndex];
    FCurve.Enabled:=Value;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetZoom(Value: TFloat);
begin
  if (Value <= FMaxZoom) and (Value >= 1 / FMaxZoom) then
  begin
    FZoom:=Value;
    XAxis.SetZoom(FZoom);
    YAxis.SetZoom(FZoom);
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.ClearMarkBox;
begin
  if Assigned(CPBmp) then
  begin
    Canvas.CopyRect(CPRect,CPBmp.Canvas,Rect(0,0,8,8));
    CPBmp.Free;
    CPBmp:=nil;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DrawMarkBox;
var
  I,J: Integer;
begin
  if Assigned(CPBmp) then ClearMarkBox;
  I:=FPositions.FXAxisLeft + FXAxis.Pixel(CPx);
  J:=Height - Positions.YAxisBottom - FYAxis.Pixel(CPy);
  CPRect:=Rect(I - 3,J - 3,I + 4,J + 4);
  CPBmp:=TBitMap.Create;
  CPBmp.Width:=8;
  CPBmp.Height:=8;
  CPBmp.Canvas.CopyRect(Rect(0,0,8,8),Canvas,CPRect);
  Canvas.Pen.Color:=clWhite;
  Canvas.Brush.Color:=clWhite;
  HClip:=CreateRectRgn(Positions.XAxisLeft,Positions.YAxisTop,
                       Width - Positions.XAxisRight,Height - Positions.YAxisBottom);
  SelectClipRgn(Canvas.Handle,HClip);

  if FMode = gmCursor then
  begin
    Canvas.MoveTo(CPRect.Left + 1,CPRect.Top + 1);
    Canvas.LineTo(CPRect.Right,CPRect.Bottom);
    Canvas.MoveTo(CPRect.Right - 1,CPRect.Top + 1);
    Canvas.LineTo(CPRect.Left,CPRect.Bottom);
  end
  else Canvas.FrameRect(CPRect);

  DeleteObject(HClip);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DrawMark(ACanvas: TCanvas; MarkType: TMarkType;
                            MarkColor: TColor; MarkSize: TMarkSize; X,Y: Integer);
begin
  ACanvas.Pen.Color:=MarkColor;
  ACanvas.Brush.Style:=bsClear;
  case MarkType of
    mtBox: begin
             ACanvas.MoveTo(X - MarkSize,Y - MarkSize);
             ACanvas.LineTo(X + MarkSize,Y - MarkSize);
             ACanvas.LineTo(X + MarkSize,Y + MarkSize);
             ACanvas.LineTo(X - MarkSize,Y + MarkSize);
             ACanvas.LineTo(X - MarkSize,Y - MarkSize);
           end;
    mtCircle: ACanvas.Ellipse(X - MarkSize,Y - MarkSize,X + MarkSize + 2,Y + MarkSize + 2);
    mtCross: begin
               ACanvas.MoveTo(X - MarkSize + 1,Y - MarkSize + 1);
               ACanvas.LineTo(X + MarkSize,Y + MarkSize);
               ACanvas.MoveTo(X + MarkSize - 1,Y - MarkSize + 1);
               ACanvas.LineTo(X - MarkSize,Y + MarkSize);
             end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.CheckCurvePoints(X,Y: Integer);
var
  I,J,K,L: Integer;
  Px,Py,Lx,Ly,Dx,Dy,MaxXDif,MaxYDif: TFloat;
begin
  ClearMarkBox;
  if not Freeze or (Freeze and not CPMatch) then
  begin
    Px:=FXAxis.Value(X);
    Py:=FYAxis.Value(Y);

    MaxXDif:=10 * FXAxis.FValuePerPixel;
    MaxYDif:=10 * FYAxis.FValuePerPixel;

    CPMatch:=False;
    J:=Pred(FCurveList.Count);
    for I:=0 to J do
    begin
      FCurve:=FCurveList.Items[I];
      if FCurve.FEnabled then
      begin
        K:=Pred(FCurve.FPoints.Count);
        for L:=0 to K do
        begin
          GetPoint(I,L,Lx,Ly);
          Dx:=Abs(Px - Lx);
          Dy:=Abs(Py - Ly);
          if not CPMatch then
          begin
            CPMatch:=(Dx < MaxXDif) and (Dy < MaxYDif);
            if CPMatch then
            begin
              CPx:=Lx;
              CPy:=Ly;
              CPCurve:=I;
              CPIndex:=L;
            end;
          end
          else
          begin
            if (Dx < Abs(Px - CPx)) and (Dy < MaxYDif) or
               (Dy < Abs(Py - CPy)) and (Dx < MaxXDif) then
            begin
              CPx:=Lx;
              CPy:=Ly;
              CPCurve:=I;
              CPIndex:=L;
            end;
          end;
        end;
      end;
    end;
  end;
  if CPMatch then DrawMarkBox;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.ShowHintPanel(Show: Boolean);
begin
  FHintPanel.Visible:=Show;
  if Assigned(FControls.FHintPanel) then FControls.FHintPanel.Checked:=Show;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.ChangeCPx(Fx: TFloat);
begin
  if (FMode = gmMove) and CPMatch then
  begin
    GetPoint(CPCurve,CPIndex,CPx,CPy);
    ChangePoint(CPCurve,CPIndex,Fx,CPy);
    CPx:=Fx;
    OutXY(CPx,CPy);
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.ChangeCPy(Fy: TFloat);
begin
  if (FMode = gmMove) and CPMatch then
  begin
    GetPoint(CPCurve,CPIndex,CPx,CPy);
    ChangePoint(CPCurve,CPIndex,CPx,Fy);
    CPy:=Fy;
    OutXY(CPx,CPy);
    Paint;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.ChangeCurveOfs(Ox,Oy: TFloat; Relative: Boolean);
var
  N: Integer;
begin
  if Assigned(FControls.FPanListBox) then
    for N:=0 to Pred(FControls.FPanListBox.Items.Count) do
  begin
    if FControls.FPanListBox.Checked[N] then
    begin
      FCurve:=FCurveList.Items[N];
      if FCurve.FEnabled then
      begin
        if Relative then
        begin
          if Ox > -9.99E15 then SetXOfs(N,GetXOfs(N) + Ox);
          if Oy > -9.99E15 then SetYOfs(N,GetYOfs(N) + Oy);
        end
        else
        begin
          if Ox > -9.99E15 then SetXOfs(N,Ox);
          if Oy > -9.99E15 then SetYOfs(N,Oy);
        end;
        OutXY(GetXOfs(N),GetYOfs(N));
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.GetCPInfo(var CPMatch: Boolean; var CPCurve,CPIndex: Integer);
begin
  CPMatch:=CPMatch;
  CPCurve:=CPCurve;
  CPIndex:=CPIndex;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoXEditExit(Sender: TObject);
var
  F: TFloat;
begin
  if not AtoF(TEdit(FControls.FXIn).Text,F) then Exit;
  ChangeCPx(F);
  ChangeCurveOfs(F,-9.9E16,False);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoYEditExit(Sender: TObject);
var
  F: TFloat;
begin
  if not AtoF(TEdit(FControls.FYIn).Text,F) then Exit;
  ChangeCPy(F);
  ChangeCurveOfs(-9.9E16,F,False);
end;
{------------------------------------------------------------------------------}

constructor THintPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Graph:=TXYGraph(AOwner);
  FStrings:=TStringList.Create;
  FStrings.OnChange:=DoStringsChange;
  SetBounds(5,5,0,0);
  Cursor:=crHandPoint;
  Moving:=False;
  Start:=True;
end;
{------------------------------------------------------------------------------}

destructor THintPanel.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.Loaded;
begin
  inherited Loaded;
  Canvas.Font.Name:='MS Sans Serif';
  Canvas.Font.Size:=8;
  Canvas.Font.Color:=clBlack;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.NewBounds;
var
  H,I,J,L,N,W: Integer;
begin
  J:=0;
  N:=0;
  if FStrings.Count > 0 then
  begin
    for I:=0 to Pred(FStrings.Count) do
    begin
      L:=Canvas.TextWidth(FStrings.Strings[I]);
      if L > J then J:=L;
      if Length(FStrings.Strings[I]) > 0 then Inc(N);
    end;

    H:=14 * N + 4;
    W:=J + 4;

    if (N > 0) and (N <= MaxHintLines) then
    begin
      Width:=W + 4;
      Height:=H;
    end;
  end;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.DoStringsChange(Sender: TObject);
begin
  NewBounds;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
 // Anchors:=[];
  MouseX:=X;
  MouseY:=Y;
  Graph.ClearMarkBox;
  Graph.CPMatch:=False;
  Moving:=True;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  Dx,Dy: Integer;
begin
  inherited MouseMove(Shift,X,Y);
  if ssLeft in Shift then
  begin
    Dx:=X - MouseX;
    Dy:=Y - MouseY;
    SetBounds(Left + Dx,Top + Dy,Width,Height);
  end;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited MouseUp(Button,Shift,X,Y);
//  Anchors:=[akRight,akTop];
  Moving:=False;
end;
{------------------------------------------------------------------------------}

procedure THintPanel.Paint;
var
  I,L,N: Integer;
begin
  inherited Paint;

  if Start then Start:=False;

  if FStrings.Count > 0 then
  begin
    N:=0;
    for I:=0 to Pred(FStrings.Count) do
    begin
      L:=Length(FStrings.Strings[I]);
      if (L > 0) and (N <= MaxHintLines) then Inc(N);
    end;

    for I:=0 to Pred(N) do Canvas.TextOut(2,2 + I * 14,FStrings.Strings[I]);
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.SetMode(Value: TMode);
begin
  FMode:=Value;
  case FMode of
    gmNone: if Assigned(FControls.FModeNone) then
              FControls.FModeNone.Checked:=True;
    gmMove: if Assigned(FControls.FModeMove) then
              FControls.FModeMove.Checked:=True;
    gmInsert: if Assigned(FControls.FModeInsert) then
                FControls.FModeInsert.Checked:=True;
    gmDelete: if Assigned(FControls.FModeDelete) then
                FControls.FModeDelete.Checked:=True;
    gmCursor: if Assigned(FControls.FModeCursor) then
                FControls.FModeCursor.Checked:=True;
  end;
end;
{------------------------------------------------------------------------------}
{
procedure TXYGraph.SetEditMode(Value: TEditMode);
begin
  FEditMode:=Value;
  if (FEditMode = emMove) or (FEditMode = emInsert) then
  begin
    if Assigned(FControls.XOut) then FControls.XOut.Enabled:=True;
    if Assigned(FControls.YOut) then FControls.YOut.Enabled:=True;
  end
  else
  begin
    if Assigned(FControls.XOut) then FControls.XOut.Enabled:=False;
    if Assigned(FControls.YOut) then FControls.YOut.Enabled:=False;
  end;
  case FEditMode of
    emNone: if Assigned(FControls.FEditModeNone) then
              FControls.FEditModeNone.Checked:=True;
    emMove: if Assigned(FControls.FEditModeMove) then
              FControls.FEditModeMove.Checked:=True;
    emInsert: if Assigned(FControls.FEditModeInsert) then
                FControls.FEditModeInsert.Checked:=True;
    emDelete: if Assigned(FControls.FEditModeDelete) then
                FControls.FEditModeDelete.Checked:=True;
  end;
  if FEditMode <> emNone then SetMode(gmNone);
end;
}
{------------------------------------------------------------------------------}
(*
procedure TXYGraph.SetPanMode(Value: TPanMode);
begin
  FPanMode:=Value;
  case FPanMode of
    pmGraph: if Assigned(FControls.FPanModeGraph) then
               FControls.FPanModeGraph.Checked:=True;
    pmCurves: if Assigned(FControls.FPanModeCurves) then
              begin
                FControls.FPanModeCurves.Checked:=True;
                if Assigned(FControls.XOut) then FControls.XOut.Enabled:=True;
                if Assigned(FControls.YOut) then FControls.YOut.Enabled:=True;
              end;
  end;
end;
*)
{------------------------------------------------------------------------------}

procedure TXYGraph.DoButtonClick(Sender: TObject);
begin
  if Sender = FControls.FClear then
  begin
    while FCurveList.Count > 0 do DeleteCurve(0);
    FHintPanel.FStrings.Clear;
    ShowHintPanel(False);
    GraphTitle:='Graph-Title';
    if Assigned(FControls.FViewListBox) then FControls.FViewListBox.Items.Clear;
    if Assigned(FControls.FPanListBox) then FControls.FPanListBox.Items.Clear;
    Reset;
    Application.ProcessMessages;
    Paint;
  end
  else if (Sender = FControls.FOpenView) and Assigned(FControls.FViewListBox) then
  begin
    FControls.FViewListBox.BringToFront;
    FControls.FViewListBox.Visible:=not FControls.FViewListBox.Visible;
  end
  else if (Sender = FControls.FOpenPan) and Assigned(FControls.FPanListBox) then
  begin
    FControls.FPanListBox.BringToFront;
    FControls.FPanListBox.Visible:=not FControls.FPanListBox.Visible;
  end
  else if Sender = FControls.FReset then Reset;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoRadioButtonClick(Sender: TObject);
begin
  if Sender = FControls.FModeNone then FMode:=gmNone
  else if Sender = FControls.FModeMove then FMode:=gmMove
  else if Sender = FControls.FModeInsert then FMode:=gmInsert
  else if Sender = FControls.FModeDelete then FMode:=gmDelete
  else if Sender = FControls.FModeCursor then FMode:=gmCursor;
  SetEditEnable(FMode = gmMove);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoCheckBoxClick(Sender: TObject);
begin
  if Sender = FControls.FAspectRatio then
    ZoomAspectRatio:=FControls.FAspectRatio.Checked
  else if Sender = FControls.FMainGrid then
  begin
    FXAxis.ShowMainGrid:=FControls.FMainGrid.Checked;
    FYAxis.ShowMainGrid:=FControls.FMainGrid.Checked;
  end
  else if Sender = FControls.FSubGrid then
  begin
    FXAxis.ShowSubGrid:=FControls.FSubGrid.Checked;
    FYAxis.ShowSubGrid:=FControls.FSubGrid.Checked;
  end
  else if Sender = FControls.FHintPanel then
    FHintPanel.Visible:=FControls.FHintPanel.Checked;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DoListBoxClickCheck(Sender: TObject);
var
  LB: TCheckListBox;
  N: Integer;

begin
  if Sender = FControls.FViewListBox then
  begin
    LB:=FControls.FViewListBox;
    for N:=0 to Pred(LB.Items.Count) do
    begin
      FCurve:=FCurveList.Items[N];
      FCurve.Enabled:=LB.Checked[N];
    end;
    Paint;
  end
  else if Sender = FControls.FPanListBox then
  begin
    PanCurves:=False;
    N:=0;
    if Assigned(FControls.FPanListBox) then
      while not PanCurves and (N < FControls.FPanListBox.Items.Count) do
    begin
      PanCurves:=FControls.FPanListBox.Checked[N];
      Inc(N);
    end;
    if FMode <> gmMove then SetEditEnable(PanCurves);
  end;
end;
{------------------------------------------------------------------------------}

constructor TControls.Create(AGraph: TXYGraph);
begin
  inherited Create;
  Graph:=AGraph;
  FXOut:=nil;
  FYOut:=nil;
  FMode:=nil;
  FCurve:=nil;
  FItem:=nil;
  FColor:=nil;
  FAngle:=nil;
  FXIn:=nil;
  FYIn:=nil;
  FClear:=nil;
  FOpenView:=nil;
  FOpenPan:=nil;
  FReset:=nil;
  FModeNone:=nil;
  FModeMove:=nil;
  FModeInsert:=nil;
  FModeDelete:=nil;
  FModeCursor:=nil;
  FAspectRatio:=nil;
  FMainGrid:=nil;
  FSubGrid:=nil;
  FHintPanel:=nil;
  FViewListBox:=nil;
  FPanListBox:=nil;
end;
{------------------------------------------------------------------------------}

procedure TControls.SetControl(Index: Integer; Value: TControl);
begin
  case Index of
    0: FXOut:=Value;
    1: FYOut:=Value;
    2: FMode:=Value;
    3: FCurve:=Value;
    4: FItem:=Value;
    5: FColor:=Value;
    6: FAngle:=Value;
  end;
end;
{------------------------------------------------------------------------------}

procedure TControls.SetEdit(Index: Integer; Value: TEdit);
begin
  case Index of
    0: begin
         FXIn:=Value;
         if Assigned(FXIn) then FXIn.OnExit:=Graph.DoXEditExit;
       end;
    1: begin
         FYIn:=Value;
         if Assigned(FYIn) then FYIn.OnExit:=Graph.DoYEditExit;
       end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TControls.SetButton(Index: Integer; Value: TButton);
begin
  case Index of
    0: begin
         FClear:=Value;
         if Assigned(FClear) then FClear.OnClick:=Graph.DoButtonClick;
       end;
    1: begin
         begin
           FOpenView:=Value;
           if Assigned(FOpenView) then FOpenView.OnClick:=Graph.DoButtonClick;
         end;
       end;
    2: begin
         FOpenPan:=Value;
         if Assigned(FOpenPan) then FOpenPan.OnClick:=Graph.DoButtonClick;
       end;
    3: begin
         FReset:=Value;
         if Assigned(FReset) then FReset.OnClick:=Graph.DoButtonClick;
       end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TControls.SetRadioButton(Index: Integer; Value: TRadioButton);
begin
  case Index of
    0: begin
         FModeNone:=Value;
         if Assigned(FModeNone) then
           FModeNone.OnClick:=Graph.DoRadioButtonClick;
       end;
    1: begin
         FModeMove:=Value;
         if Assigned(FModeMove) then
           FModeMove.OnClick:=Graph.DoRadioButtonClick;
       end;
    2: begin
         FModeInsert:=Value;
         if Assigned(FModeInsert) then
           FModeInsert.OnClick:=Graph.DoRadioButtonClick;
       end;
    3: begin
         FModeDelete:=Value;
         if Assigned(FModeDelete) then
           FModeDelete.OnClick:=Graph.DoRadioButtonClick;
       end;
    4: begin
         FModeCursor:=Value;
         if Assigned(FModeCursor) then
           FModeCursor.OnClick:=Graph.DoRadioButtonClick;
       end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TControls.SetCheckBox(Index: Integer; Value: TCheckBox);
begin
  case Index of
    0: begin
         FAspectRatio:=Value;
         if Assigned(FAspectRatio) then
           FAspectRatio.OnClick:=Graph.DoCheckBoxClick;
       end;
    1: begin
         FMainGrid:=Value;
         if Assigned(FMainGrid) then
           FMainGrid.OnClick:=Graph.DoCheckBoxClick;
       end;
    2: begin
         FSubGrid:=Value;
         if Assigned(FSubGrid) then
           FSubGrid.OnClick:=Graph.DoCheckBoxClick;
       end;
    3: begin
         FHintPanel:=Value;
         if Assigned(FHintPanel) then
           FHintPanel.OnClick:=Graph.DoCheckBoxClick;
       end;
  end;
end;
{------------------------------------------------------------------------------}

procedure TControls.SetListBox(Index: Integer; Value: TCheckListBox);
begin
  case Index of
    0: begin
         FViewListBox:=Value;
         if Assigned(FViewListBox) then
           FViewListBox.OnClickCheck:=Graph.DoListBoxClickCheck;
       end;
    1: begin
         FPanListBox:=Value;
         if Assigned(FPanListBox) then
           FPanListBox.OnClickCheck:=Graph.DoListBoxClickCheck;
       end;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.SaveCurveToStream(FileStream: TFileStream; Item: Integer): Boolean;
var
  CurveData: TCurveData;
  N: Integer;
begin
  Result:=False;
  if not InRange(Item,0,Pred(FCurveList.Count)) or not Assigned(FileStream) then Exit;
  FCurve:=FCurveList.Items[Item];
  try
    CurveData.Name:=FCurve.Name;
    CurveData.Enabled:=FCurve.Enabled;
    CurveData.Color:=FCurve.Color;
    CurveData.LineWidth:=FCurve.LineWidth;
    CurveData.PenStyle:=FCurve.PenStyle;
    CurveData.Points:=FCurve.FPoints.Count;
    CurveData.Texts:=FCurve.FTexts.Count;
    CurveData.Marks:=FCurve.FMarks.Count;
    CurveData.XOfs:=FCurve.XOfs;
    CurveData.YOfs:=FCurve.YOfs;
    CurveData.FontName:=FCurve.FFont.Name;
    CurveData.FontSize:=FCurve.FFont.Size;
    CurveData.FontStyle:=FCurve.FFont.Style;
    CurveData.MarkSize:=FCurve.MarkSize;

    FileStream.Write(CurveData,SizeOf(TCurveData));

    for N:=0 to Pred(FCurve.FPoints.Count) do
      FileStream.Write(FCurve.FPoints.Items[N]^,SizeOf(TPointRec));

    for N:=0 to Pred(FCurve.FTexts.Count) do
      FileStream.Write(FCurve.FTexts.Items[N]^,SizeOf(TTextRec));

    for N:=0 to Pred(FCurve.FMarks.Count) do
      FileStream.Write(FCurve.FMarks.Items[N]^,SizeOf(TMarkRec));

    Result:=True;
  except
    ShowMessage('Error writing stream!');
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.LoadCurveFromStream(FileStream: TFileStream): Boolean;
var
  CurveData: TCurveData;
  PointRec: TPointRec;
  TextRec: TTextRec;
  MarkRec: TMarkRec;
  H,N: Integer;
begin
  Result:=False;
  if not Assigned(FileStream) then Exit;
  try
    FileStream.Read(CurveData,SizeOf(TCurveData));
    H:=MakeCurve(CurveData.Name,CurveData.Color,CurveData.LineWidth,
                 CurveData.PenStyle,CurveData.Enabled);
    SetXOfs(H,CurveData.XOfs);
    SetYOfs(H,CurveData.YOfs);
    SetCurveFont(H,CurveData.FontName,CurveData.FontSize,CurveData.FontStyle);
    SetMarkSize(H,CurveData.MarkSize);

    for N:=0 to Pred(CurveData.Points) do
    begin
      FileStream.Read(PointRec,SizeOf(TPointRec));
      AddPoint(H,PointRec.x,PointRec.y);
    end;

    for N:=0 to Pred(CurveData.Texts) do
    begin
      FileStream.Read(TextRec,SizeOf(TTextRec));
      AddText(H,TextRec.PointIndex,TextRec.XOfs,TextRec.YOfs,
              TextRec.Text,TextRec.TextColor);
    end;

    for N:=0 to Pred(CurveData.Marks) do
    begin
      FileStream.Read(MarkRec,SizeOf(TMarkRec));
      AddMark(H,MarkRec.PointIndex,MarkRec.MarkType,MarkRec.MarkColor);
    end;

    Result:=True;
  except
    ShowMessage('Error reading stream!');
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.SaveCurveToFile(const FileName: string; Item: Integer): Boolean;
var
  FileStream: TFileStream;
begin
  Result:=False;
  try
    FileStream:=TFileStream.Create(FileName,fmCreate);
    try
      FileStream.Position:=0;
      Result:=SaveCurveToStream(FileStream,Item);
    except
      Result:=False;
    end;
  finally
    FileStream.Free;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.LoadCurveFromFile(const FileName: string): Boolean;
var
  FileStream: TFileStream;
begin
  Result:=False;
  if not FileExists(FileName) then Exit;
  try
    FileStream:=TFileStream.Create(FileName,fmOpenRead);
    try
      FileStream.Position:=0;
      Result:=LoadCurveFromStream(FileStream);
    except
      Result:=False;
    end;
  finally
    FileStream.Free;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.SaveGraphToFile(const FileName: string): Boolean;
var
  FileStream: TFileStream;
  GraphData: TGraphData;
  FontRec: TFontRec;
  N: Integer;
begin
  Result:=False;
  try
    FileStream:=TFileStream.Create(FileName,fmCreate);
    try
      GraphData.GraphTitle:=FGraphTitle;
      GraphData.Curves:=FCurveList.Count;
      GraphData.Zoom:=FZoom;
      GraphData.MaxZoom:=FMaxZoom;
      FileStream.Position:=0;
      FileStream.Write(GraphData,SizeOf(GraphData));
      FileStream.Write(FXAxis.FTitle,FXAxis.InstanceSize);
      FileStream.Write(FYAxis.FTitle,FYAxis.InstanceSize);
      FileStream.Write(FColors.FAxisBkGnd,FColors.InstanceSize);
      FileStream.Write(FPositions.FXAxisLeft,FPositions.InstanceSize);

      FontRec.AxisScaleFontName:=FFonts.FAxisScale.Name;
      FontRec.AxisScaleFontSize:=FFonts.FAxisScale.Size;
      FontRec.AxisScaleFontStyle:=FFonts.FAxisScale.Style;
      FontRec.AxisTitleFontName:=FFonts.FAxisTitle.Name;
      FontRec.AxisTitleFontSize:=FFonts.FAxisTitle.Size;
      FontRec.AxisTitleFontStyle:=FFonts.FAxisTitle.Style;
      FontRec.GraphTitleFontName:=FFonts.FGraphTitle.Name;
      FontRec.GraphTitleFontSize:=FFonts.FGraphTitle.Size;
      FontRec.GraphTitleFontStyle:=FFonts.FGraphTitle.Style;
      FileStream.Write(FontRec,SizeOf(FontRec));

      for N:=0 to Pred(GraphData.Curves) do SaveCurveToStream(FileStream,N);
      FHintPanel.FStrings.SaveToStream(FileStream);
      Result:=True;
    except
      Result:=False;
    end;
  finally
    FileStream.Free;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.LoadGraphFromFile(const FileName: string): Boolean;
var
  FileStream: TFileStream;
  GraphData: TGraphData;
  FontRec: TFontRec;
  N: Integer;
begin
  Result:=False;
  if not FileExists(FileName) then Exit;
  try
    FileStream:=TFileStream.Create(FileName,fmOpenRead);
    try
      FileStream.Position:=0;
      FileStream.Read(GraphData,SizeOf(GraphData));
      FGraphTitle:=GraphData.GraphTitle;
      FZoom:=GraphData.Zoom;
      FMaxZoom:=GraphData.MaxZoom;
      FileStream.Read(FXAxis.FTitle,FXAxis.InstanceSize);
      FileStream.Read(FYAxis.FTitle,FYAxis.InstanceSize);
      FileStream.Read(FColors.FAxisBkGnd,FColors.InstanceSize);
      FileStream.Read(FPositions.FXAxisLeft,FPositions.InstanceSize);

      FileStream.Read(FontRec,SizeOf(FontRec));
      FFonts.FAxisScale.Name:=FontRec.AxisScaleFontName;
      FFonts.FAxisScale.Size:=FontRec.AxisScaleFontSize;
      FFonts.FAxisScale.Style:=FontRec.AxisScaleFontStyle;
      FFonts.FAxisTitle.Name:=FontRec.AxisTitleFontName;
      FFonts.FAxisTitle.Size:=FontRec.AxisTitleFontSize;
      FFonts.FAxisTitle.Style:=FontRec.AxisTitleFontStyle;
      FFonts.FGraphTitle.Name:=FontRec.GraphTitleFontName;
      FFonts.FGraphTitle.Size:=FontRec.GraphTitleFontSize;
      FFonts.FGraphTitle.Style:=FontRec.GraphTitleFontStyle;

      for N:=0 to Pred(GraphData.Curves) do LoadCurveFromStream(FileStream);
      FHintPanel.FStrings.LoadFromStream(FileStream);
      FXAxis.CalcAxis;
      FYAxis.CalcAxis;
      FHintPanel.Paint;
      Paint;
      Result:=True;
    except
      Result:=False;
    end;
  finally
    FileStream.Free;
  end;
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DXFAxis;
var
  Dif,MainStep,SubStep: TFloat;
  TickLen,TextWidth: TFloat;
  N,M: Integer;
  S: Str32;
begin
  if not Assigned(DXFOut) then Exit;
  TextWidth:=DXFOut.TextHeight / 4 * 3;
  TickLen:=5;
  Dif:=FXAxis.FMax - FXAxis.FMin;
  if FXAxis.MainTicks <> 0 then MainStep:=Dif / FXAxis.MainTicks else MainStep:=Dif;
  if FXAxis.SubTicks <> 0 then SubStep:=MainStep / FXAxis.SubTicks else SubStep:=MainStep;
  for N:=0 to FXAxis.MainTicks do
  begin
    DXFOut.Line(DXFOut.ToX(FXAxis.FMin + N * MainStep),
                DXFOut.ToY(FYAxis.FMin) - TickLen,0,
                DXFOut.ToX(FXAxis.FMin + N * MainStep),
                DXFOut.ToY(FYAxis.FMax),0);
    S:=FloatToStrF(FXAxis.Min + (N * FXAxis.ValuePerMainTick),
                   ffFixed,7,FXAxis.Decimals);
    for M:=1 to Length(S) do if S[M] = ' ' then Delete(S,M,1);
    DXFOut.DText(DXFOut.ToX(FXAxis.FMin + N * MainStep) - TextWidth * Length(S) / 2,
                 DXFOut.ToY(FYAxis.FMin) - DXFOut.TextHeight - TickLen - 1,0,
                 DXFOut.TextHeight,0,S);
    if FXAxis.FShowSubGrid and (N < FXAxis.MainTicks) then
      for M:=1 to Pred(FXAxis.SubTicks) do
    begin
      DXFOut.Line(DXFOut.ToX(FXAxis.FMin + N * MainStep + M * SubStep),
                  DXFOut.ToY(FYAxis.FMin),0,
                  DXFOut.ToX(FXAxis.FMin + N * MainStep + M * SubStep),
                  DXFOut.ToY(FYAxis.FMax),0);
    end;
  end;

  Dif:=FYAxis.FMax - FYAxis.FMin;
  if FYAxis.MainTicks <> 0 then MainStep:=Dif / FYAxis.MainTicks else MainStep:=Dif;
  if FYAxis.SubTicks <> 0 then SubStep:=MainStep / FYAxis.SubTicks else SubStep:=MainStep;
  for N:=0 to FYAxis.MainTicks do
  begin
    DXFOut.Line(DXFOut.ToX(FXAxis.FMin) - TickLen,
                DXFOut.ToY(FYAxis.FMin + N * MainStep),0,
                DXFOut.ToX(FXAxis.FMax),
                DXFOut.ToY(FYAxis.FMin + N * MainStep),0);
    S:=FloatToStrF(FYAxis.Min + (N * FYAxis.ValuePerMainTick),
                   ffFixed,7,FYAxis.Decimals);
    for M:=1 to Length(S) do if S[M] = ' ' then Delete(S,M,1);
    DXFOut.DText(DXFOut.ToX(FXAxis.FMin) - TickLen - TextWidth * Length(S) - 1,
                 DXFOut.ToY(FYAxis.FMin + N * MainStep) - DXFOut.TextHeight / 2,
                 0,DXFOut.TextHeight,0,S);
    if FXAxis.FShowSubGrid and (N < FYAxis.MainTicks) then
      for M:=1 to Pred(FYAxis.SubTicks) do
    begin
      DXFOut.Line(DXFOut.ToX(FXAxis.FMin),
                  DXFOut.ToY(FYAxis.FMin + N * MainStep + M * SubStep),0,
                  DXFOut.ToX(FXAxis.FMax),
                  DXFOut.ToY(FYAxis.FMin + N * MainStep + M * SubStep),0);
    end;
  end;

  S:=FXAxis.FTitle;
  SubStep:=Length(S) * TextWidth;
  MainStep:=(FXAxis.FMax - FXAxis.FMin) / 2;
  Dif:=FXAxis.FMin + MainStep - SubStep;
  DXFOut.DText(DXFOut.ToX(Dif),
               DXFOut.ToY(FYAxis.FMin) - TickLen - DXFOut.TextHeight * 6,
               0,DXFOut.TextHeight * 2,0,S);

  S:=FYAxis.FTitle;
  SubStep:=Length(S) * TextWidth;
  MainStep:=(FYAxis.FMax - FYAxis.FMin) / 2;
  Dif:=FYAxis.FMin + MainStep - SubStep;
  DXFOut.DText(DXFOut.ToX(FXAxis.FMin) - TickLen - TextWidth * Length(S) -
               DXFOut.TextHeight * 2 - 2,
               DXFOut.ToY(Dif),0,DXFOut.TextHeight * 2,90,S);

  S:=FGraphTitle;
  SubStep:=Length(S) * TextWidth;
  MainStep:=(FXAxis.FMax - FXAxis.FMin) / 2;
  Dif:=FXAxis.FMin + MainStep - SubStep;
  DXFOut.DText(DXFOut.ToX(Dif),
               DXFOut.ToY(FYAxis.FMax) + DXFOut.TextHeight * 6,
               0,DXFOut.TextHeight * 2,0,S);
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.DXFCurves;
var
  H,I,J: Integer;
  X,Y: TFloat;
begin
  for H:=0 to Pred(FCurveList.Count) do
  begin
    FCurve:=FCurveList.Items[H];
    if FCurve.Enabled and (FCurve.FPoints.Count > 0) then
    begin
      J:=Pred(FCurve.FPoints.Count);
      DXFOut.StartPolyLine(False);
      for I:=0 to J do
      begin
        FCurve.GetPoint(I,X,Y);
        DXFOut.Vertex(DXFOut.ToX(X),DXFOut.ToY(Y),0);
      end;
      DXFOut.EndPolyLine;
    end;
  end;
end;
{------------------------------------------------------------------------------}

function TXYGraph.MakeDXF(const FileName: string; FromX1,FromY1,FromX2,FromY2,
                          ToX1,ToY1,ToX2,ToY2,TextHeight: TFloat;
                          Decimals: Byte): Boolean;
begin
  Result:=False;
  try
    DXFOut:=TDXFOut.Create(FromX1,FromY1,FromX2,FromY2,ToX1,ToY1,ToX2,ToY2,
                           TextHeight,Decimals);
    try
      DXFOut.Header;
      DXFAxis;
      DXFCurves;
      DXFOut.Trailer;
      DXFOut.StringList.SaveToFile(FileName);
      Result:=True;
    except
      Result:=False;
    end;
  finally
    DXFOut.Free;
  end;
end;
{------------------------------------------------------------------------------}

constructor TDXFOut.Create(AFromXMin,AFromYMin,AFromXMax,AFromYMax,
                           AToXMin,AToYMin,AToXMax,AToYMax,ATextHeight: TFloat; ADecimals: Byte);
begin
  inherited Create;
  FromXMin:=AFromXMin;
  FromYMin:=AFromYMin;
  FromXMax:=AFromXMax;
  FromYMax:=AFromYMax;
  ToXMin:=AToXMin;
  ToYMin:=AToYMin;
  ToXMax:=AToXMax;
  ToYMax:=AToYMax;
  TextHeight:=ATextHeight;
  Decimals:=ADecimals;
  StringList:=TStringList.Create;
end;
{------------------------------------------------------------------------------}

destructor TDXFOut.Destroy;
begin
  StringList.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.Header;
begin
  LayerName:='0';
  StringList.Add('0');
  StringList.Add('SECTION');
  StringList.Add('2');
  StringList.Add('HEADER');
  StringList.Add('9');
  StringList.Add('$LIMMIN');
  StringList.Add('10');
  StringList.Add(FToA(ToXMin));
  StringList.Add('20');
  StringList.Add(FToA(ToYMin));
  StringList.Add('9');
  StringList.Add('$LIMMAX');
  StringList.Add('10');
  StringList.Add(FToA(ToXMax));
  StringList.Add('20');
  StringList.Add(FToA(ToYMax));
  StringList.Add('0');
  StringList.Add('ENDSEC');
  StringList.Add('0');
  StringList.Add('SECTION');
  StringList.Add('2');
  StringList.Add('TABLES');
  StringList.Add('0');
  StringList.Add('TABLE');
  StringList.Add('2');
  StringList.Add('LAYER');
  StringList.Add('70');
  StringList.Add('1');
  StringList.Add('0');
  StringList.Add('LAYER');
  StringList.Add('2');
  StringList.Add('0');
  StringList.Add('70');
  StringList.Add('64');
  StringList.Add('62');
  StringList.Add('7');
  StringList.Add('6');
  StringList.Add('CONTINUOUS');
  StringList.Add('0');
  StringList.Add('ENDTAB');
  StringList.Add('0');
  StringList.Add('ENDSEC');
  StringList.Add('0');
  StringList.Add('SECTION');
  StringList.Add('2');
  StringList.Add('ENTITIES');
end;
{------------------------------------------------------------------------------}

function TDXFOut.FToA(F: TFloat): Str32;
var
  I: Integer;
begin
  Result:=FloatToStrF(F,ffFixed,16,Decimals);
  I:=Pos(',',Result);
  if I > 0 then Result[I]:='.';
end;
{------------------------------------------------------------------------------}

function TDXFOut.ToX(X: TFloat): TFloat;
var
  Factor,FromDif: TFloat;
begin
  FromDif:=FromXMax - FromXMin;
  if FromDif <> 0.0 then Factor:=(ToXMax - ToXMin) / FromDif else Factor:=1.0;
  Result:=X * Factor;
end;
{------------------------------------------------------------------------------}

function TDXFOut.ToY(Y: TFloat): TFloat;
var
  Factor,FromDif: TFloat;
begin
  FromDif:=FromYMax - FromYMin;
  if FromDif <> 0.0 then Factor:=(ToYMax - ToYMin) / FromDif else Factor:=1.0;
  Result:=Y * Factor;
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.SetLayer(const Name: Str32);
begin
  LayerName:=Name;
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.Layer;
begin
  StringList.Add('8');
  StringList.Add(LayerName);
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.StartPoint(X,Y,Z: TFloat);
begin
  StringList.Add('10');
  StringList.Add(FToA(X));
  StringList.Add('20');
  StringList.Add(FToA(Y));
  StringList.Add('30');
  StringList.Add(FToA(Z));
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.EndPoint(X,Y,Z: TFloat);
begin
  StringList.Add('11');
  StringList.Add(FToA(X));
  StringList.Add('21');
  StringList.Add(FToA(Y));
  StringList.Add('31');
  StringList.Add(FToA(Z));
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.AddText(const Txt: Str32);
begin
  StringList.Add('1');
  StringList.Add(Txt);
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.StartPolyLine(Closed: Boolean);
var
  Flag : Byte;
begin
  StringList.Add('0');
  StringList.Add('POLYLINE');
  Layer;
  StringList.Add('66');
  StringList.Add('1');
  StartPoint(0,0,0);
  Flag:=8;
  if Closed then Flag:=Flag or 1;
  StringList.Add('70');
  StringList.Add(IntToStr(Flag));
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.Vertex(X,Y,Z: TFloat);
var
 Flag : Byte;
begin
  StringList.Add('0');
  StringList.Add('VERTEX');
  Layer;
  StartPoint(X,Y,Z);
  StringList.Add('70');
  Flag:=32;
  StringList.Add(IntToStr(Flag));
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.EndPolyLine;
begin
  StringList.Add('0');
  StringList.Add('SEQEND');
  Layer;
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.Line(X1,Y1,Z1,X2,Y2,Z2: TFloat);
begin
  StringList.Add('0');
  StringList.Add('LINE');
  Layer;
  StartPoint(X1,Y1,Z1);
  EndPoint(X2,Y2,Z2);
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.Point(X,Y,Z: TFloat);
begin
  StringList.Add('0');
  StringList.Add('POINT');
  Layer;
  StartPoint(X,Y,Z);
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.DText(X,Y,Z,Height,Angle: TFloat; const Txt: Str32);
begin
  StringList.Add('0');
  StringList.Add('TEXT');
  Layer;
  StartPoint(X,Y,Z);
  StringList.Add('40');
  StringList.Add(FToA(Height));
  AddText(Txt);
  StringList.Add('50');
  StringList.Add(FToA(Angle));
end;
{------------------------------------------------------------------------------}

procedure TDXFOut.Trailer;
begin
  StringList.Add('0');
  StringList.Add('ENDSEC');
  StringList.Add('0');
  StringList.Add('EOF');
end;
{------------------------------------------------------------------------------}

procedure TXYGraph.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component,Operation);

  if Operation = opRemove then
  begin
    if Component = FControls.FXOut then FControls.FXOut:=nil;
    if Component = FControls.FYOut then FControls.FYOut:=nil;
    if Component = FControls.FMode then FControls.FMode:=nil;
    if Component = FControls.FCurve then FControls.FCurve:=nil;
    if Component = FControls.FItem then FControls.FItem:=nil;
    if Component = FControls.FColor then FControls.FColor:=nil;
    if Component = FControls.FAngle then FControls.FAngle:=nil;
    if Component = FControls.FXIn then FControls.FXIn:=nil;
    if Component = FControls.FYIn then FControls.FYIn:=nil;
    if Component = FControls.FClear then FControls.FClear:=nil;
    if Component = FControls.FOpenView then FControls.FOpenView:=nil;
    if Component = FControls.FOpenPan then FControls.FOpenPan:=nil;
    if Component = FControls.FReset then FControls.FReset:=nil;
    if Component = FControls.FModeNone then FControls.FModeNone:=nil;
    if Component = FControls.FModeMove then FControls.FModeMove:=nil;
    if Component = FControls.FModeInsert then FControls.FModeInsert:=nil;
    if Component = FControls.FModeDelete then FControls.FModeDelete:=nil;
    if Component = FControls.FModeCursor then FControls.FModeCursor:=nil;
    if Component = FControls.FAspectRatio then FControls.FAspectRatio:=nil;
    if Component = FControls.FMainGrid then FControls.FMainGrid:=nil;
    if Component = FControls.FSubGrid then FControls.FSubGrid:=nil;
    if Component = FControls.FHintPanel then FControls.FHintPanel:=nil;
    if Component = FControls.FViewListBox then FControls.FViewListBox:=nil;
    if Component = FControls.FPanListBox then FControls.FPanListBox:=nil;
  end;
end;
{------------------------------------------------------------------------------}

initialization
end.

