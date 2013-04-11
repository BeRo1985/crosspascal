unit Tree;
{$i Compiler.inc}

interface

uses BeRoUtils,Globals,Error,Symbols,HugeString;

type TTreeNodeType=(ttntEmpty,
                    ttntSubRange,ttntAssign,
                    ttntArrayConstructor,
                    ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual,
                    ttntAdd,ttntSub,ttntOr,ttntXor,ttntIn,
                    ttntMul,ttntSlash,ttntDiv,ttntMod,ttntAnd,ttntShl,ttntShr,ttntMinus,ttntNot,
                    ttntNil,ttntFloat,
                    ttntBlock,ttntStatement,ttntASM,
                    ttntVAR,ttntTYPE,ttntTYPEINFO,ttntTYPECONV,ttntTYPECHECK,
                    ttntFOR,ttntWHILE,ttntREPEAT,ttntIF,ttntWITH,
                    ttntCASE,ttntCASEBLOCK,ttntCASEValue,
                    ttntBREAK,ttntCONTINUE,ttntEXIT,ttntFAIL,
                    ttntLABEL,ttntGOTO,
                    ttntTRY,ttntTRYONELSE,ttntRAISE,
                    ttntPROCEDURE,ttntCALL,ttntParameter,ttntIndex,ttntPointer,
                    ttntAddress,ttntField,
                    ttntORDConst,ttntCHARConst,ttntSTRINGConst,ttntFloatConst,
                    ttntSETConst,ttntPCHARConst,
                    ttntCCODE,ttntCEXPRESSION,ttntCBLOCK,ttntPASCALBLOCK,ttntCLOCATION,
                    ttntTEMPOBJECT);

     TTreeNodeTypes=set of TTreeNodeType;

     TTreeManager=class;

     TTreeNodeItemType=(ttnitUndefined,ttnitTemporary,ttnitMemoryReference,ttnitOrdinalConstant,
                        ttnitMemory,ttnitLabel);

     TTreeNode=class
      public
       Left,Right,Block,ElseTree,ExceptTree,FinallyTree:TTreeNode;
       TreeManager:TTreeManager;
       LineNumber,WithLevel:longint;
       FileName:ansistring;
       TreeNodeType:TTreeNodeType;
       Symbol,MethodSymbol,SymbolField,ParameterSymbol:PSymbol;
       Return,CheckType,CompareType,InheritedType,WithType:PType;
       ItemType:TTreeNodeItemType;
       ItemValue:int64;
       Signed,Forced,Colon,IsDownTo,ReferenceParameter,DoNotOptimize,Warning500:boolean;
       Value:int64;
       CharValue:THugeChar;
       StringData:THugeString;
       FloatValue:extended;
       SetData:TSetArray;
       LabelName:ansistring;
       constructor Create(var TheTreeManager:TTreeManager);
       constructor CreateFrom(var TheTree:TTreeNode);
       destructor Destroy; override;
       procedure Clear;
       procedure Assign(From:TTreeNode);
       function CheckForNodes(const TreeTypes:TTreeNodeTypes):boolean;
     end;

     TTreeManager=class
      private
       Options:POptions;
       procedure DumpNode(TreeNode:TTreeNode;TreeName:ansistring;Level:longint);
      public
       Tree:TTreeNode;
       Error:TError;
       SymbolManager:TSymbolManager;
       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheOptions:POptions);
       destructor Destroy; override;
       procedure Clear;
       procedure Dump(TreeNode:TTreeNode;TreeName:ansistring='Root');
       function NewNode:TTreeNode;
       function ReverseTree(TreeNode:TTreeNode):TTreeNode;
       function GenerateLeftRightNode(T:TTreeNodeType;Left,Right:TTreeNode):TTreeNode;
       function GenerateLeftNode(T:TTreeNodeType;Left:TTreeNode):TTreeNode;
       function GenerateVarNode(Symbol:PSymbol):TTreeNode;
       function GenerateTypeNode(AType:PType):TTreeNode;
       function GenerateTypeInfoNode(AType:PType):TTreeNode;
       function GenerateTypeConvNode(Left:TTreeNode;CastedType:PType;Forced:boolean):TTreeNode;
       function GenerateTypeCheckNode(Left:TTreeNode;CheckType:PType):TTreeNode;
       function GeneratePointerNode(Left:TTreeNode;PointerToType:PType):TTreeNode;
       function GenerateIndexNode(Symbol:PSymbol;Left:TTreeNode):TTreeNode;
       function GenerateFieldNode(Symbol,SymbolField:PSymbol;Left:TTreeNode):TTreeNode;
       function GenerateCallNode(Symbol:PSymbol):TTreeNode;
       function GenerateMethodCallNode(Symbol,MethodSymbol:PSymbol;Right:TTreeNode;InheritedType:PType):TTreeNode;
       function GenerateOrdConstNode(Value:int64;AType:PType):TTreeNode;
       function GenerateCharConstNode(C:THugeChar;AType:PType):TTreeNode;
       function GenerateFloatConstNode(FloatValue:extended;AType:PType):TTreeNode;
       function GenerateStringConstNode(const S:THugeString;AType:PType):TTreeNode;
       function GeneratePCharConstNode(const S:THugeString;AType:PType):TTreeNode;
       function GenerateSetConstNode(const S:TSetArray;AType:PType):TTreeNode;
       function GenerateCCodeNode(Left:TTreeNode):TTreeNode;
       function GenerateCExpressionNode(Left:TTreeNode):TTreeNode;
       function GenerateCBlockNode(const S:THugeString):TTreeNode;
       function GeneratePascalBlockNode(Right:TTreeNode):TTreeNode;
       function GenerateCLocationNode(const S:THugeString;AType:PType):TTreeNode;
       function GenerateTempObjectNode(AType:PType):TTreeNode;
       function GenerateNilNode(AType:PType):TTreeNode;
       function GenerateEmptyNode:TTreeNode;
//     function GenerateAsmNode(ASMBlock:TAsmList):TTreeNode;
       function GenerateIfNode(Left,Right,ElseTree:TTreeNode):TTreeNode;
       function GenerateTryNode(Block,ExceptTree,FinallyTree:TTreeNode):TTreeNode;
       function GenerateTryOnElseNode(Symbol:PSymbol;CheckType:PType;OnTree,ElseTree:TTreeNode):TTreeNode;
       function GenerateRaiseNode(Left,Right:TTreeNode):TTreeNode;
       function GenerateCaseNode(Left,Right,ElseTree:TTreeNode):TTreeNode;
       function GenerateCaseBlockNode(Left,Right,Block:TTreeNode):TTreeNode;
       function GenerateCaseValNode(Left,Right,ElseTree:TTreeNode):TTreeNode;
       function GenerateRepeatNode(Left,Right:TTreeNode):TTreeNode;
       function GenerateWhileNode(Left,Right:TTreeNode):TTreeNode;
       function GenerateBreakNode:TTreeNode;
       function GenerateContinueNode:TTreeNode;
       function GenerateExitNode:TTreeNode;
       function GenerateFailNode:TTreeNode;
       function GenerateGotoNode(Symbol:PSymbol):TTreeNode;
       function GenerateLabelNode(Symbol:PSymbol):TTreeNode;
       function GenerateForNode(Left,Right,Block:TTreeNode;IsDownTo:boolean):TTreeNode;
       function GenerateParameterNode(Expression,Next:TTreeNode):TTreeNode;
       function GenerateWithNode(Expression,Block:TTreeNode):TTreeNode;
     end;

implementation

uses DebugLogUtils;

constructor TTreeNode.Create(var TheTreeManager:TTreeManager);
begin
 inherited Create;
 TreeManager:=TheTreeManager;
 Clear;
end;

constructor TTreeNode.CreateFrom(var TheTree:TTreeNode);
begin
 inherited Create;
 Clear;
 Assign(TheTree);
end;

destructor TTreeNode.Destroy;
begin
 if assigned(Left) then begin
  Left.Destroy;
 end;
 if assigned(Right) then begin
  Right.Destroy;
 end;
 if assigned(Block) then begin
  Block.Destroy;
 end;
 if assigned(ElseTree) then begin
  ElseTree.Destroy;
 end;
 if assigned(ExceptTree) then begin
  ExceptTree.Destroy;
 end;
 if assigned(FinallyTree) then begin
  FinallyTree.Destroy;
 end;
 Clear;
 inherited Destroy;
end;

procedure TTreeNode.Clear;
begin
 Left:=nil;
 Right:=nil;
 Block:=nil;
 ElseTree:=nil;
 ExceptTree:=nil;
 FinallyTree:=nil;
 LineNumber:=-1;
 WithLevel:=-1;
 FileName:='';
 TreeNodeType:=ttntEmpty;
 Symbol:=nil;
 MethodSymbol:=nil;
 SymbolField:=nil;
 ParameterSymbol:=nil;
 Return:=nil;
//CastedType:=NIL;
 CheckType:=nil;
 CompareType:=nil;
 InheritedType:=nil;
 WithType:=nil;
 ItemType:=ttnitUndefined;
 ItemValue:=0;
 Signed:=false;
 Forced:=false;
 Colon:=false;
 IsDownTo:=false;
 ReferenceParameter:=false;
 DoNotOptimize:=false;
 Warning500:=false;
 Value:=0;
 CharValue:=0;
 StringData:=nil;
 FloatValue:=0;
 FILLCHAR(SetData,sizeof(TSetArray),#0);
 LabelName:='';
end;

procedure TTreeNode.Assign(From:TTreeNode);
begin
 if assigned(From) then begin
  TreeManager:=From.TreeManager;
  Left:=nil;
  Right:=nil;
  Block:=nil;
  ElseTree:=nil;
  if assigned(From.Left) then begin
   Left:=TTreeNode.CreateFrom(From.Left);
  end;
  if assigned(From.Right) then begin
   Right:=TTreeNode.CreateFrom(From.Right);
  end;
  if assigned(From.Block) then begin
   Block:=TTreeNode.CreateFrom(From.Block);
  end;
  if assigned(From.ElseTree) then begin
   ElseTree:=TTreeNode.CreateFrom(From.ElseTree);
  end;
  LineNumber:=From.LineNumber;
  WithLevel:=From.WithLevel;
  FileName:=From.FileName;
  TreeNodeType:=From.TreeNodeType;
  Symbol:=From.Symbol;
  MethodSymbol:=From.MethodSymbol;
  SymbolField:=From.SymbolField;
  ParameterSymbol:=From.ParameterSymbol;
  Return:=From.Return;
//CastedType:=From.CastedType;
  CheckType:=From.CheckType;
  CompareType:=From.CompareType;
  InheritedType:=From.InheritedType;
  WithType:=From.WithType;
  ItemType:=From.ItemType;
  ItemValue:=From.ItemValue;
  Signed:=From.Signed;
  Forced:=From.Forced;
  Colon:=From.Colon;
  IsDownTo:=From.IsDownTo;
  ReferenceParameter:=From.ReferenceParameter;
  Value:=From.Value;
  CharValue:=From.CharValue;
  if length(From.StringData)=0 then begin
   StringData:=nil;
  end else begin
   StringData:=copy(From.StringData);
  end;
  FloatValue:=From.FloatValue;
  SetData:=From.SetData;
  LabelName:=From.LabelName;
 end;
end;

function TTreeNode.CheckForNodes(const TreeTypes:TTreeNodeTypes):boolean;
begin
 result:=TreeNodeType in TreeTypes;
 if result then exit;
 if assigned(Left) then begin
  result:=Left.CheckForNodes(TreeTypes);
  if result then exit;
 end;
 if assigned(Right) then begin
  result:=Right.CheckForNodes(TreeTypes);
  if result then exit;
 end;
 if assigned(Block) then begin
  result:=Block.CheckForNodes(TreeTypes);
  if result then exit;
 end;
 if assigned(ElseTree) then begin
  result:=ElseTree.CheckForNodes(TreeTypes);
  if result then exit;
 end;
end;

constructor TTreeManager.Create(TheError:TError;TheSymbolManager:TSymbolManager;TheOptions:POptions);
begin
 inherited Create;
 Options:=TheOptions;
 Tree:=nil;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 Clear;
end;

destructor TTreeManager.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TTreeManager.Clear;
begin
 if assigned(Tree) then Tree.Destroy;
 Tree:=nil;
end;

procedure TTreeManager.DumpNode(TreeNode:TTreeNode;TreeName:ansistring;Level:longint);
 procedure WriteTree(S:ansistring);
 var Counter:longint;
 begin
  for Counter:=1 to Level-1 do S:=' '+S;
  DebugLog(S);
 end;
 procedure WriteType(S:ansistring);
 var Counter:longint;
 begin
  for Counter:=1 to Level do S:=' '+S;
  DebugLog(S);
 end;
 procedure WriteData(S:ansistring);
 var Counter:longint;
 begin
  for Counter:=1 to Level+1 do S:=' '+S;
  DebugLog(S);
 end;
 procedure DumpBlock;
 var SubTreeNode:TTreeNode;
 begin
  SubTreeNode:=TreeNode;
  while assigned(SubTreeNode) do begin
   DumpNode(SubTreeNode.Right,'BLOCKSTATEMENT',Level+2);
   SubTreeNode:=SubTreeNode.Left;
  end;
 end;
begin
 if not (DebugLogActive and DebugLogDumpTree) then begin
  exit;
 end;
 if assigned(TreeNode) then begin
  WriteTree(TreeName);
  case TreeNode.TreeNodeType of
   ttntEMPTY:begin
    WriteType('EMPTY');
   end;
   ttntSubRange:begin
    WriteType('SUBRANGE');
   end;
   ttntAssign:begin
    WriteType('ASSIGN');
   end;
   ttntArrayConstructor:begin
    WriteType('ARRAYCONSTRUCTOR');
   end;
   ttntLess:begin
    WriteType('LESS');
   end;
   ttntLessOrEqual:begin
    WriteType('LESSOREQUAL');
   end;
   ttntGreater:begin
    WriteType('GREATER');
   end;
   ttntGreaterOrEqual:begin
    WriteType('GREATEROREQUAL');
   end;
   ttntEqual:begin
    WriteType('EQUAL');
   end;
   ttntNotEqual:begin
    WriteType('NOTEQUAL');
   end;
   ttntAdd:begin
    WriteType('ADD');
   end;
   ttntSUB:begin
    WriteType('SUB');
   end;
   ttntOr:begin
    WriteType('OR');
   end;
   ttntXor:begin
    WriteType('XOR');
   end;
   ttntIn:begin
    WriteType('IN');
   end;
   ttntMul:begin
    WriteType('MUL');
   end;
   ttntSlash:begin
    WriteType('SLASH');
   end;
   ttntDiv:begin
    WriteType('DIV');
   end;
   ttntMod:begin
    WriteType('MOD');
   end;
   ttntAnd:begin
    WriteType('AND');
   end;
   ttntShl:begin
    WriteType('SHL');
   end;
   ttntShr:begin
    WriteType('SHR');
   end;
   ttntMinus:begin
    WriteType('MINUS');
   end;
   ttntNot:begin
    WriteType('NOT');
   end;
   ttntNil:begin
    WriteType('NIL');
   end;
   ttntFloat:begin
    WriteType('FLOAT');
   end;
   ttntBlock:begin
    WriteType('BLOCK');
   end;
   ttntStatement:begin
    WriteType('STATEMENT');
   end;
   ttntASM:begin
    WriteType('ASM');
   end;
   ttntVAR:begin
    WriteType('VAR');
    if assigned(TreeNode.Symbol) then begin
     WriteData('Variable: '+CorrectSymbolName(TreeNode.Symbol^.name));
    end;
   end;
   ttntTYPE:begin
    WriteType('TYPE');
   end;
   ttntTYPEINFO:begin
    WriteType('TYPEINFO');
   end;
   ttntTYPECONV:begin
    WriteType('TYPECONV');
   end;
   ttntTYPECHECK:begin
    WriteType('TYPECHECK');
   end;
   ttntFOR:begin
    WriteType('FOR');
   end;
   ttntWHILE:begin
    WriteType('WHILE');
   end;
   ttntREPEAT:begin
    WriteType('REPEAT');
   end;
   ttntIF:begin
    WriteType('IF');
   end;
   ttntWITH:begin
    WriteType('WITH');
   end;
   ttntCASE:begin
    WriteType('CASE');
   end;
   ttntCASEBLOCK:begin
    WriteType('CASEBLOCK');
   end;
   ttntCASEValue:begin
    WriteType('CASEVALUE');
   end;
   ttntBREAK:begin
    WriteType('BREAK');
   end;
   ttntCONTINUE:begin
    WriteType('CONTINUE');
   end;
   ttntEXIT:begin
    WriteType('EXIT');
   end;
   ttntFAIL:begin
    WriteType('FAIL');
   end;
   ttntLABEL:begin
    WriteType('LABEL');
   end;
   ttntGOTO:begin
    WriteType('GOTO');
   end;
   ttntTRY:begin
    WriteType('TRY');
   end;
   ttntTRYONELSE:begin
    WriteType('TRYONELSE');
   end;
   ttntRAISE:begin
    WriteType('RAISE');
   end;
   ttntPROCEDURE:begin
    WriteType('PROCEDURE');
   end;
   ttntCALL:begin
    WriteType('CALL');
    if assigned(TreeNode.Symbol) then begin
     WriteData('Symbol: '+CorrectSymbolName(TreeNode.Symbol^.name));
    end;
   end;
   ttntParameter:begin
    WriteType('PARAMETER');
   end;
   ttntIndex:begin
    WriteType('INDEX');
   end;
   ttntPointer:begin
    WriteType('POINTER');
   end;
   ttntAddress:begin
    WriteType('ADDRESS');
   end;
   ttntField:begin
    WriteType('FIELD');
   end;
   ttntORDConst:begin
    WriteType('ORDCONST');
    WriteData('Ordinal Value: '+INTTOSTR(TreeNode.Value));
   end;
   ttntCHARConst:begin
    WriteType('CHARCONST');
    WriteData('Char Value: '+HugeStringToWideString(HugeStringConcat(nil,TreeNode.CharValue)));
   end;
   ttntSTRINGConst:begin
    WriteType('STRINGCONST');
    WriteData('String Data: '+HugeStringToWideString(TreeNode.StringData));
   end;
   ttntFloatConst:begin
    WriteType('FLOATCONST');
    WriteData('Float Value: '+FLOATTOSTR(TreeNode.FloatValue));
   end;
   ttntSETConst:begin
    WriteType('SETCONST');
   end;
   ttntPCHARConst:begin
    WriteType('PCHARCONST');
    WriteData('String Data: '+HugeStringToWideString(TreeNode.StringData));
   end;
   ttntCCODE:begin
    WriteType('CCODE');
   end;
   ttntCEXPRESSION:begin
    WriteType('CEXPRESSION');
   end;
   ttntCBLOCK:begin
    WriteType('CBLOCK');
    WriteData('C block: '+HugeStringToWideString(TreeNode.StringData));
   end;
   ttntPASCALBLOCK:begin
    WriteType('PASCALBLOCK');
   end;
   ttntCLOCATION:begin
    WriteType('CLOCATION');
    WriteData('C location: '+HugeStringToWideString(TreeNode.StringData));
   end;
   ttntTEMPOBJECT:begin
    WriteType('TEMPOBJECT');
   end;
  end;
  if TreeNode.TreeNodeType=ttntBLOCK then begin
   DumpBlock;
  end else begin
   DumpNode(TreeNode.Left,'Left',Level+2);
   DumpNode(TreeNode.Right,'Right',Level+2);
  end;
  DumpNode(TreeNode.ElseTree,'ElseTree',Level+2);
  DumpNode(TreeNode.ExceptTree,'ExceptTree',Level+2);
  DumpNode(TreeNode.FinallyTree,'FinallyTree',Level+2);
  DumpNode(TreeNode.Block,'Block',Level+2);
 end;
end;

procedure TTreeManager.Dump(TreeNode:TTreeNode;TreeName:ansistring='Root');
begin
 DumpNode(TreeNode,TreeName,1);
end;

function TTreeManager.NewNode:TTreeNode;
begin
 if assigned(Tree) then begin
  result:=Tree;
  Tree:=Tree.Left;
 end else begin
  result:=TTreeNode.Create(self);
 end;
 result.Clear;
 result.LineNumber:=Error.CurrentLine;
 result.FileName:=Error.CurrentFileName;
end;

function TTreeManager.ReverseTree(TreeNode:TTreeNode):TTreeNode;
var CurrentTreeNode,NewTreeNode:TTreeNode;
begin
 CurrentTreeNode:=nil;
 while assigned(TreeNode) do begin
  NewTreeNode:=TreeNode;
  TreeNode:=TreeNode.Right;
  NewTreeNode.Right:=CurrentTreeNode;
  CurrentTreeNode:=NewTreeNode;
 end;
 result:=CurrentTreeNode;
end;

function TTreeManager.GenerateLeftRightNode(T:TTreeNodeType;Left,Right:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=T;
 result.Left:=Left;
 result.Right:=Right;
end;

function TTreeManager.GenerateLeftNode(T:TTreeNodeType;Left:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=T;
 result.Left:=Left;
end;

function TTreeManager.GenerateVarNode(Symbol:PSymbol):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntVAR;
 result.Symbol:=Symbol;
 if assigned(Symbol) then begin
  result.Return:=Symbol^.TypeDefinition;
 end;
end;

function TTreeManager.GenerateTypeNode(AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTYPE;
 result.Return:=AType;
end;

function TTreeManager.GenerateTypeInfoNode(AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTYPEINFO;
 result.Return:=AType;
end;

function TTreeManager.GenerateTypeConvNode(Left:TTreeNode;CastedType:PType;Forced:boolean):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTYPECONV;
 result.Left:=Left;
//RESULT.CastedType:=CastedType;
 result.Return:=CastedType;
 result.Forced:=Forced;
end;

function TTreeManager.GenerateTypeCheckNode(Left:TTreeNode;CheckType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTYPECHECK;
 result.Left:=Left;
 result.CheckType:=CheckType;
end;

function TTreeManager.GeneratePointerNode(Left:TTreeNode;PointerToType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntPOINTER;
 result.Left:=Left;
 result.Return:=PointerToType;
end;

function TTreeManager.GenerateIndexNode(Symbol:PSymbol;Left:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntIndex;
 result.Symbol:=Symbol;
 result.Left:=Left;
end;

function TTreeManager.GenerateFieldNode(Symbol,SymbolField:PSymbol;Left:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntField;
 result.Symbol:=Symbol;
 result.SymbolField:=SymbolField;
 result.Left:=Left;
 if assigned(SymbolField) then begin
  result.Return:=SymbolField^.TypeDefinition;
 end;
end;

function TTreeManager.GenerateCallNode(Symbol:PSymbol):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCall;
 result.Symbol:=Symbol;
 if assigned(Symbol) then begin
  result.Return:=Symbol^.TypeDefinition;
 end;
end;

function TTreeManager.GenerateMethodCallNode(Symbol,MethodSymbol:PSymbol;Right:TTreeNode;InheritedType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCall;
 result.Symbol:=Symbol;
 result.MethodSymbol:=MethodSymbol;
 result.InheritedType:=InheritedType;
 result.Right:=Right;
 if assigned(MethodSymbol) then begin
  result.Return:=MethodSymbol^.TypeDefinition;
 end;
end;

function TTreeManager.GenerateOrdConstNode(Value:int64;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntOrdConst;
 result.Value:=Value;
 result.Return:=AType;
end;

function TTreeManager.GenerateCharConstNode(C:THugeChar;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCharConst;
 result.CharValue:=C;
 result.Return:=AType;
end;

function TTreeManager.GenerateFloatConstNode(FloatValue:extended;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntFloatConst;
 result.FloatValue:=FloatValue;
 result.Return:=AType;
end;

function TTreeManager.GenerateStringConstNode(const S:THugeString;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntStringConst;
 result.StringData:=S;
 result.Return:=AType;
end;

function TTreeManager.GeneratePCharConstNode(const S:THugeString;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntPCharConst;
 result.StringData:=S;
 result.Return:=AType;
end;

function TTreeManager.GenerateSetConstNode(const S:TSetArray;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntSetConst;
 result.SetData:=S;
 result.Return:=AType;
end;       

function TTreeManager.GenerateCCodeNode(Left:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCCODE;
 result.Left:=Left;
 result.Return:=nil;
end;

function TTreeManager.GenerateCExpressionNode(Left:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCEXPRESSION;
 result.Left:=Left;
 result.Return:=SymbolManager.TypeCExpression;
end;

function TTreeManager.GenerateCBlockNode(const S:THugeString):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCBLOCK;
 result.StringData:=S;
 result.Return:=SymbolManager.TypeCExpression;
end;

function TTreeManager.GeneratePascalBlockNode(Right:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntPASCALBLOCK;
 result.Right:=Right;
end;

function TTreeManager.GenerateCLocationNode(const S:THugeString;AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCLOCATION;
 result.StringData:=S;
 result.Return:=AType;
end;

function TTreeManager.GenerateTempObjectNode(AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTEMPOBJECT;
 result.Return:=AType;
end;

function TTreeManager.GenerateNilNode(AType:PType):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntOrdConst;
 result.Value:=0;
 result.Return:=AType;
end;

function TTreeManager.GenerateEmptyNode:TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntEmpty;
end;

{FUNCTION TTreeManager.GenerateAsmNode(ASMBlock:TAsmList):TTreeNode;
BEGIN
 RESULT:=NewNode;
 RESULT.TreeNodeType:=ttntASM;
 RESULT.ASMBlock:=ASMBlock;
END;}

function TTreeManager.GenerateIfNode(Left,Right,ElseTree:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntIF;
 result.Left:=Left;
 result.Right:=Right;
 result.ElseTree:=ElseTree;
end;

function TTreeManager.GenerateTryNode(Block,ExceptTree,FinallyTree:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTRY;
 result.Block:=Block;
 result.ExceptTree:=ExceptTree;
 result.FinallyTree:=FinallyTree;
end;

function TTreeManager.GenerateTryOnElseNode(Symbol:PSymbol;CheckType:PType;OnTree,ElseTree:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntTRYONELSE;
 result.Symbol:=Symbol;
 result.CheckType:=CheckType;
 result.Left:=OnTree;
 result.Right:=ElseTree;
end;

function TTreeManager.GenerateRaiseNode(Left,Right:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntRAISE;
 result.Left:=Left;
 result.Right:=Right;
end;

function TTreeManager.GenerateCaseNode(Left,Right,ElseTree:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCASE;
 result.Left:=Left;
 result.Right:=Right;
 result.ElseTree:=ElseTree;
end;

function TTreeManager.GenerateCaseBlockNode(Left,Right,Block:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCaseBlock;
 result.Left:=Left;
 result.Right:=Right;
 result.Block:=Block;
end;

function TTreeManager.GenerateCaseValNode(Left,Right,ElseTree:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCaseValue;
 result.Left:=Left;
 result.Right:=Right;
 result.ElseTree:=ElseTree;
end;

function TTreeManager.GenerateRepeatNode(Left,Right:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntREPEAT;
 result.Left:=Left;
 result.Right:=Right;
end;

function TTreeManager.GenerateWhileNode(Left,Right:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntWHILE;
 result.Left:=Left;
 result.Right:=Right;
end;

function TTreeManager.GenerateBreakNode:TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntBREAK;
end;             

function TTreeManager.GenerateContinueNode:TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntCONTINUE;
end;

function TTreeManager.GenerateExitNode:TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntEXIT;
end;

function TTreeManager.GenerateFailNode:TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntFAIL;
end;

function TTreeManager.GenerateGotoNode(Symbol:PSymbol):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntGOTO;
 result.LabelName:=Symbol^.name;
end;

function TTreeManager.GenerateLabelNode(Symbol:PSymbol):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntLABEL;
 result.Symbol:=Symbol;
 result.LabelName:=Symbol^.name;
end;

function TTreeManager.GenerateForNode(Left,Right,Block:TTreeNode;IsDownTo:boolean):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntFOR;
 result.Left:=Left;
 result.Right:=Right;
 result.Block:=Block;
 result.IsDownTo:=IsDownTo;
end;

function TTreeManager.GenerateWithNode(Expression,Block:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntWITH;
 result.Left:=Expression;
 result.Right:=Block;
end;

function TTreeManager.GenerateParameterNode(Expression,Next:TTreeNode):TTreeNode;
begin
 result:=NewNode;
 result.TreeNodeType:=ttntParameter;
 result.Left:=Expression;
 result.Right:=Next;
 result.Colon:=false;
end;

end.
