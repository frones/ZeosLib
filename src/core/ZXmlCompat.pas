unit ZXmlCompat;

{$I ZCore.inc}

interface

{$IFDEF FPC}

uses
  Classes, SysUtils, dom;

type
  IXMLNodeList = interface;

  IXMLNode = interface(IUnknown)
  ['{D960E088-0897-41C4-83C7-0E4F1F315630}']
    function GetChildNodes: IXMLNodeList;
    function GetAttribute(AttrName: DOMString): OleVariant;
    property ChildNodes: IXMLNodeList read GetChildNodes;
    property Attributes[const AttrName: DOMString]: OleVariant read GetAttribute;
  end;

  IXMLNodeList = interface(IUnknown)
  ['{01CDB783-479A-4204-962B-EBC52D749737}']
    function Get(Index: LongWord): IXMLNode;
    procedure Delete(Index: LongWord);
    function FindNode(NodeName: DOMString): IXMLNode;
    function Count: Cardinal;
  end;

  IXmlDocument = interface(IUnknown)
  ['{123AE59F-7756-4548-9DD8-A6053E09EBC1}']
    procedure LoadFromStream(Stream: TStream);
    function GetChildNodes: IXMLNodeList;
    property ChildNodes: IXMLNodeList read GetChildNodes;
  end;

  TZXmlDocument = class(TInterfacedObject, IXmlDocument)
    protected
      FXmlDoc: TXmlDocument;
    public
      procedure LoadFromStream(Stream: TStream);
      function GetChildNodes: IXMLNodeList;
  end;

  TZXmlNode = class(TInterfacedObject, IXmlNode)
    protected
      FNode: TDOMNode;
    public
      constructor Create(Node: TDOMNode);
      function GetChildNodes: IXMLNodeList;
      function GetAttribute(AttrName: DOMString): OleVariant;
  end;

  TZXmlNodeList = class(TInterfacedObject, IXMLNodeList)
    protected
      FParentNode: TDOMNode;
    public
      constructor Create(ParentNode: TDOMNode);
      function Get(Index: LongWord): IXMLNode;
      procedure Delete(Index: LongWord);
      function FindNode(NodeName: DOMString): IXMLNode;
      function Count: LongWord;
  end;

{$ENDIF}

implementation

{$IFDEF FPC}

uses xmlread, Variants;

procedure TZXmlDocument.LoadFromStream(Stream: TStream);
begin
  ReadXMLFile(FXmlDoc, Stream);
end;

function TZXmlDocument.GetChildNodes: IXMLNodeList;
begin
  Result := TZXmlNodeList.Create(FXmlDoc) as IXMLNodeList;
end;

constructor TZXmlNodeList.Create(ParentNode: TDOMNode);
begin
  inherited Create;
  FParentNode := ParentNode;
end;

function TZXmlNodeList.Get(Index: LongWord): IXMLNode;
begin
  Result := TZXmlNode.Create(FParentNode.ChildNodes.Item[Index]);
end;

procedure TZXmlNodeList.Delete(Index: LongWord);
var
  Node: TDOMNode;
begin
  Node := FParentNode.ChildNodes[Index];
  FParentNode.RemoveChild(Node);
  FreeAndNil(Node);
end;

function TZXmlNodeList.FindNode(NodeName: DOMString): IXMLNode;
var
  Node: TDOMNode;
begin
  Node := FParentNode.FindNode(NodeName);
  if Assigned(node) then
    Result := TZXmlNode.Create(Node) as IXMLNode
  else
    Result := nil;
end;

function TZXmlNodeList.Count: LongWord;
begin
  Result := FParentNode.ChildNodes.Count;
end;

constructor TZXmlNode.Create(Node: TDOMNode);
begin
  inherited Create;
  FNode := Node;
end;

function TZXmlNode.GetChildNodes: IXMLNodeList;
begin
  Result := TZXmlNodeList.Create(FNode) as IXMLNodeList;
end;

function TZXmlNode.GetAttribute(AttrName: DOMString): OleVariant;
var
  Node: TDOMNode;
begin
  Node := FNode.Attributes.GetNamedItem(AttrName);
  if Assigned(Node) then
    Result := Node.NodeValue
  else
    Result := Null;
end;

{$ENDIF}

end.

