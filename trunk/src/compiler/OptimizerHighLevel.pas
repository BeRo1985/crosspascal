unit OptimizerHighLevel;
{$i Compiler.inc}

interface

uses SysUtils,BeRoStream,Globals,Symbols,Error,Tree,HugeString;

type TOptimizerHighLevel=class
      private
       Options:POptions;
       Error:TError;
       SymbolManager:TSymbolManager;
       TreeManager:TTreeManager;
       Initialized:boolean;
       TypeFloat,TypeBoolean,TypePointer,TypeSigned32Bit,TypeSigned64Bit,TypeChar,
       TypeAnsiString,TypeWideString,TypeHugeString:PType;
       procedure OptimizeParameter(var TreeNode:TTreeNode;Symbol:PSymbol);
       procedure OptimizeInternalRoutines(var TreeNode:TTreeNode);
      public
       ModuleSymbol:PSymbol;
       LocalSwitches:PLocalSwitches;
       CurrentObjectClass:PType;
       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheTreeManager:TTreeManager;TheOptions:POptions;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;
       procedure Initialize;
       procedure MaybeTypeConversion(var Left,Right:TTreeNode);
       function PrecalculateConstants(var TreeNode:TTreeNode):boolean;
       procedure OptimizeNothing(var TreeNode:TTreeNode);
       procedure OptimizeStringConst(var TreeNode:TTreeNode);
       procedure OptimizeOrdinalConst(var TreeNode:TTreeNode);
       procedure OptimizeCharConst(var TreeNode:TTreeNode);
       procedure OptimizeSetConst(var TreeNode:TTreeNode);
       procedure OptimizeFloatConst(var TreeNode:TTreeNode);
       procedure OptimizeType(var TreeNode:TTreeNode);
       procedure OptimizeTypeConv(var TreeNode:TTreeNode);
       procedure OptimizeTypeCheck(var TreeNode:TTreeNode);
       procedure OptimizeVar(var TreeNode:TTreeNode);
       procedure OptimizeAddress(var TreeNode:TTreeNode);
       procedure OptimizePointer(var TreeNode:TTreeNode);
       procedure OptimizeField(var TreeNode:TTreeNode);
       procedure OptimizeIndex(var TreeNode:TTreeNode);
       procedure OptimizeAdd(var TreeNode:TTreeNode);
       procedure OptimizeSub(var TreeNode:TTreeNode);
       procedure OptimizeOr(var TreeNode:TTreeNode);
       procedure OptimizeXor(var TreeNode:TTreeNode);
       procedure OptimizeShl(var TreeNode:TTreeNode);
       procedure OptimizeShr(var TreeNode:TTreeNode);
       procedure OptimizeCompare(var TreeNode:TTreeNode);
       procedure OptimizeWITH(var TreeNode:TTreeNode);
       procedure OptimizeAnd(var TreeNode:TTreeNode);
       procedure OptimizeIn(var TreeNode:TTreeNode);
       procedure OptimizeMul(var TreeNode:TTreeNode);
       procedure OptimizeDiv(var TreeNode:TTreeNode);
       procedure OptimizeMod(var TreeNode:TTreeNode);
       procedure OptimizeSlash(var TreeNode:TTreeNode);
       procedure OptimizeMinus(var TreeNode:TTreeNode);
       procedure OptimizeNot(var TreeNode:TTreeNode);
       procedure OptimizeSubRange(var TreeNode:TTreeNode);
       procedure OptimizeCall(var TreeNode:TTreeNode);
       procedure OptimizeRESULT(var TreeNode:TTreeNode);
       procedure OptimizeAssign(var TreeNode:TTreeNode);
       procedure OptimizeBlock(var TreeNode:TTreeNode);
       procedure OptimizeFOR(var TreeNode:TTreeNode);
       procedure OptimizeREPEAT(var TreeNode:TTreeNode);
       procedure OptimizeWHILE(var TreeNode:TTreeNode);
       procedure OptimizeIF(var TreeNode:TTreeNode);
       procedure OptimizeCASE(var TreeNode:TTreeNode);
       procedure OptimizeCASEBlock(var TreeNode:TTreeNode);
       procedure OptimizeTRY(var TreeNode:TTreeNode);
       procedure OptimizeTRYONELSE(var TreeNode:TTreeNode);
       procedure OptimizeRAISE(var TreeNode:TTreeNode);
       procedure OptimizeEmptyTrees(var TreeNode:TTreeNode);
       procedure OptimizeTree(var TreeNode:TTreeNode);
     end;

implementation

uses TypeCheck;

function IsBoolean(TreeNodeType:TTreeNodeType):boolean;
begin
 result:=TreeNodeType in [ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual];
end;

function GetType(TreeNode:TTreeNode):PType;
begin
 if assigned(TreeNode) then begin
  result:=TreeNode.Return;
  if not assigned(result) then begin
   result:=GetType(TreeNode.Left);
   if not assigned(result) then begin
    result:=GetType(TreeNode.Right);
   end;
  end;
 end else begin
  result:=nil;
 end;
end;

procedure SetCExpressionType(TreeNode:TTreeNode;ReturnType:PType);
begin
 if assigned(TreeNode) then begin
  if assigned(TreeNode.Return) and (TreeNode.Return^.TypeDefinition=ttdCEXPRESSION) then begin
   TreeNode.Return:=ReturnType;
  end;
  SetCExpressionType(TreeNode.Left,ReturnType);
  SetCExpressionType(TreeNode.Right,ReturnType);
 end;
end;

procedure CorrectCExpressionTypes(Left,Right:TTreeNode);
var ReturnType:PType;
begin
 ReturnType:=GetType(Right);
 if assigned(ReturnType) then begin
  SetCExpressionType(Left,ReturnType);
 end;
end;

constructor TOptimizerHighLevel.Create(TheError:TError;TheSymbolManager:TSymbolManager;TheTreeManager:TTreeManager;TheOptions:POptions;TheLocalSwitches:PLocalSwitches);
begin
 inherited Create;
 Options:=TheOptions;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 TreeManager:=TheTreeManager;
 LocalSwitches:=TheLocalSwitches;
 Initialized:=false;
end;

destructor TOptimizerHighLevel.Destroy;
begin
 inherited Destroy;
end;

procedure TOptimizerHighLevel.Initialize;
//VAR Symbol:PSymbol;
begin
 if not Initialized then begin
  Initialized:=true;
  TypeFloat:=SymbolManager.TypeExtended;
  TypeBoolean:=SymbolManager.TypeBoolean;
  TypePointer:=SymbolManager.TypePointer;
  TypeSigned32Bit:=SymbolManager.TypeLongint;
  TypeSigned64Bit:=SymbolManager.TypeInt64;
  TypeChar:=SymbolManager.TypeChar;
  TypeAnsiString:=SymbolManager.TypeAnsiString;
  TypeWideString:=SymbolManager.TypeWideString;
  TypeHugeString:=SymbolManager.TypeHugeString;
{ Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'EXTENDED');
  if assigned(Symbol) then begin
   TypeFloat:=Symbol^.TypeDefinition;
  end else begin
   TypeFloat:=nil;
  end;
  Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'BOOLEAN');
  if assigned(Symbol) then begin
   TypeBoolean:=Symbol^.TypeDefinition;
  end else begin
   TypeBoolean:=nil;
  end;
  Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'CARDINAL');
  if assigned(Symbol) then begin
   TypeSigned32Bit:=Symbol^.TypeDefinition;
  end else begin
   TypeSigned32Bit:=nil;
  end;
  Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'CHAR');
  if assigned(Symbol) then begin
   TypeChar:=Symbol^.TypeDefinition;
  end else begin
   TypeChar:=nil;
  end;
  TypeLongString:=nil;{}
 end;
end;

procedure TOptimizerHighLevel.MaybeTypeConversion(var Left,Right:TTreeNode);
begin
 if assigned(Left.Return) and assigned(Right.Return) and (Left.Return<>Right.Return) then begin
  if ((not (Left.TreeNodeType in [ttntCHARConst,ttntSTRINGConst])) and (not (Right.TreeNodeType in [ttntCHARConst,ttntSTRINGConst]))) and not
     (IsBoolean(Left.TreeNodeType) or IsBoolean(Right.TreeNodeType)) then begin
   if (Left.Return^.TypeDefinition=ttdSubRange) and (Right.Return^.TypeDefinition=ttdSubRange) then begin
    if SymbolManager.GetSize(Left.Return)<4 then begin
     Left:=TreeManager.GenerateTypeConvNode(Left,TypeSigned32Bit,false);
    end;
    if SymbolManager.GetSize(Right.Return)<4 then begin
     Right:=TreeManager.GenerateTypeConvNode(Right,TypeSigned32Bit,false);
    end;
    if assigned(Left.Left) then begin
     Left.LineNumber:=Left.Left.LineNumber;
    end;
    if assigned(Right.Left) then begin
     Right.LineNumber:=Right.Left.LineNumber;
    end;
    OptimizeTree(Left);
    OptimizeTree(Right);
   end;
   if assigned(Left.Return) and assigned(Right.Return) and ((Left.Return^.TypeDefinition<>Right.Return^.TypeDefinition) or (SymbolManager.GetSize(Left.Return)<>SymbolManager.GetSize(Right.Return))) then begin
    if (Left.Return^.TypeDefinition=ttdFLOAT) or (Left.TreeNodeType=ttntFLOATConst) then begin
     Right:=TreeManager.GenerateTypeConvNode(Right,Left.Return,false);
     Right.LineNumber:=Right.Left.LineNumber;
     OptimizeTree(Right);
    end else begin
     Left:=TreeManager.GenerateTypeConvNode(Left,Right.Return,false);
     Left.LineNumber:=Left.Left.LineNumber;
     OptimizeTree(Left);
    end;
   end;
  end;
 end;
end;

function TOptimizerHighLevel.PrecalculateConstants(var TreeNode:TTreeNode):boolean;
begin
 result:=false;
 if assigned(TreeNode) then begin
  Initialize;
  if (not assigned(TreeNode.Left)) or (not assigned(TreeNode.Right)) then begin
   Error.InternalError(200605180940000);
  end else if (TreeNode.Left.TreeNodeType=ttntORDConst) and (TreeNode.Right.TreeNodeType=ttntORDConst) then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
   TreeNode.Return:=nil;
   if TreeNode.Left.Return=TreeNode.Right.Return then begin
    TreeNode.Return:=TreeNode.Left.Return;
   end;
   case TreeNode.TreeNodeType of
    ttntSlash:begin
     TreeNode.FloatValue:=TreeNode.Left.Value/TreeNode.Right.Value;
     FreeAndNil(TreeNode.Left);
     FreeAndNil(TreeNode.Right);
     TreeNode.TreeNodeType:=ttntFLOATConst;
     result:=true;
     exit;
    end;
    ttntAdd:begin
     TreeNode.Value:=TreeNode.Left.Value+TreeNode.Right.Value;
    end;
    ttntSub:begin
     TreeNode.Value:=TreeNode.Left.Value-TreeNode.Right.Value;
    end;
    ttntMul:begin
     TreeNode.Value:=TreeNode.Left.Value*TreeNode.Right.Value;
    end;
    ttntDiv:begin
     TreeNode.Value:=TreeNode.Left.Value div TreeNode.Right.Value;
    end;
    ttntMod:begin
     TreeNode.Value:=TreeNode.Left.Value mod TreeNode.Right.Value;
    end;
    ttntSHL:begin
     TreeNode.Value:=TreeNode.Left.Value shl TreeNode.Right.Value;
    end;
    ttntSHR:begin
     TreeNode.Value:=TreeNode.Left.Value shr TreeNode.Right.Value;
    end;
    ttntAND:begin
     TreeNode.Value:=TreeNode.Left.Value and TreeNode.Right.Value;
    end;
    ttntXOR:begin
     TreeNode.Value:=TreeNode.Left.Value xor TreeNode.Right.Value;
    end;
    ttntOR:begin
     TreeNode.Value:=TreeNode.Left.Value or TreeNode.Right.Value;
    end;
    ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual:begin
     case TreeNode.TreeNodeType of
      ttntEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value=TreeNode.Right.Value);
      end;
      ttntNotEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value<>TreeNode.Right.Value);
      end;
      ttntGreater:begin
       TreeNode.Value:=ord(TreeNode.Left.Value>TreeNode.Right.Value);
      end;
      ttntLess:begin
       TreeNode.Value:=ord(TreeNode.Left.Value<TreeNode.Right.Value);
      end;
      ttntGreaterOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value>=TreeNode.Right.Value);
      end;
      ttntLessOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value<=TreeNode.Right.Value);
      end;
     end;
     TreeNode.Return:=TypeBoolean;
    end;
    else begin
     exit;
    end;
   end;
   if not assigned(TreeNode.Return) then begin
    TreeNode.Return:=TypeSigned32Bit;
   end;
   TreeNode.TreeNodeType:=ttntORDConst;
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
   result:=true;
  end else if (TreeNode.Left.TreeNodeType=ttntFLOATConst) and (TreeNode.Right.TreeNodeType=ttntFLOATConst) then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
   TreeNode.Return:=nil;
   case TreeNode.TreeNodeType of
    ttntAdd:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue+TreeNode.Right.FloatValue;
    end;
    ttntSub:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue-TreeNode.Right.FloatValue;
    end;
    ttntMul:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue*TreeNode.Right.FloatValue;
    end;
    ttntSlash:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue/TreeNode.Right.FloatValue;
    end;
    ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual:begin
     case TreeNode.TreeNodeType of
      ttntEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue=TreeNode.Right.FloatValue);
      end;
      ttntNotEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue<>TreeNode.Right.FloatValue);
      end;
      ttntGreater:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue>TreeNode.Right.FloatValue);
      end;
      ttntLess:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue<TreeNode.Right.FloatValue);
      end;
      ttntGreaterOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue>=TreeNode.Right.FloatValue);
      end;
      ttntLessOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue<=TreeNode.Right.FloatValue);
      end;
     end;
     TreeNode.Return:=TypeBoolean;
    end;
    else begin
     exit;
    end;
   end;
   TreeNode.TreeNodeType:=ttntFLOATConst;
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
   result:=true;
  end else if (TreeNode.Left.TreeNodeType=ttntFLOATConst) and (TreeNode.Right.TreeNodeType=ttntORDConst) then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
   TreeNode.Return:=nil;
   case TreeNode.TreeNodeType of
    ttntAdd:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue+TreeNode.Right.Value;
    end;
    ttntSub:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue-TreeNode.Right.Value;
    end;
    ttntMul:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue*TreeNode.Right.Value;
    end;
    ttntSlash:begin
     TreeNode.FloatValue:=TreeNode.Left.FloatValue/TreeNode.Right.Value;
    end;
    ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual:begin
     case TreeNode.TreeNodeType of
      ttntEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue=TreeNode.Right.Value);
      end;
      ttntNotEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue<>TreeNode.Right.Value);
      end;
      ttntGreater:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue>TreeNode.Right.Value);
      end;
      ttntLess:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue<TreeNode.Right.Value);
      end;
      ttntGreaterOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue>=TreeNode.Right.Value);
      end;
      ttntLessOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.FloatValue<=TreeNode.Right.Value);
      end;
     end;
     TreeNode.Return:=TypeBoolean;
    end;
    else begin
     exit;
    end;
   end;
   TreeNode.TreeNodeType:=ttntFLOATConst;
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
   result:=true;
  end else if (TreeNode.Left.TreeNodeType=ttntORDConst) and (TreeNode.Right.TreeNodeType=ttntFLOATConst) then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
   TreeNode.Return:=nil;
   case TreeNode.TreeNodeType of
    ttntAdd:begin
     TreeNode.FloatValue:=TreeNode.Left.Value+TreeNode.Right.FloatValue;
    end;
    ttntSub:begin
     TreeNode.FloatValue:=TreeNode.Left.Value-TreeNode.Right.FloatValue;
    end;
    ttntMul:begin
     TreeNode.FloatValue:=TreeNode.Left.Value*TreeNode.Right.FloatValue;
    end;
    ttntSlash:begin
     TreeNode.FloatValue:=TreeNode.Left.Value/TreeNode.Right.FloatValue;
    end;
    ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual:begin
     case TreeNode.TreeNodeType of
      ttntEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value=TreeNode.Right.FloatValue);
      end;
      ttntNotEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value<>TreeNode.Right.FloatValue);
      end;
      ttntGreater:begin
       TreeNode.Value:=ord(TreeNode.Left.Value>TreeNode.Right.FloatValue);
      end;
      ttntLess:begin
       TreeNode.Value:=ord(TreeNode.Left.Value<TreeNode.Right.FloatValue);
      end;
      ttntGreaterOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value>=TreeNode.Right.FloatValue);
      end;
      ttntLessOrEqual:begin
       TreeNode.Value:=ord(TreeNode.Left.Value<=TreeNode.Right.FloatValue);
      end;
     end;
     TreeNode.Return:=TypeBoolean;
    end;
    else begin
     exit;
    end;
   end;
   TreeNode.TreeNodeType:=ttntFLOATConst;
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
   result:=true;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeNothing(var TreeNode:TTreeNode);
begin
end;

procedure TOptimizerHighLevel.OptimizeStringConst(var TreeNode:TTreeNode);
begin
 TreeNode.ItemType:=ttnitMemoryReference;
 if not assigned(TreeNode.Return) then begin
  TreeNode.Return:=TypeAnsiString;
 end;
end;

procedure TOptimizerHighLevel.OptimizeOrdinalConst(var TreeNode:TTreeNode);
begin
 TreeNode.ItemType:=ttnitOrdinalConstant;
 TreeNode.ItemValue:=TreeNode.Value;
 if not assigned(TreeNode.Return) then begin
  TreeNode.Return:=TypeSigned32Bit;
 end;
end;

procedure TOptimizerHighLevel.OptimizeCharConst(var TreeNode:TTreeNode);
begin
 TreeNode.ItemType:=ttnitOrdinalConstant;
 TreeNode.ItemValue:=ord(TreeNode.CharValue);
 if not assigned(TreeNode.Return) then begin
  TreeNode.Return:=TypeChar;
{ TreeNode.Return:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass);
  TreeNode.Return.TypeDefinition:=ttdSubRange;
  TreeNode.Return.SubRangeType:=tstUnsignedChar;}
 end;
end;

procedure TOptimizerHighLevel.OptimizeSetConst(var TreeNode:TTreeNode);
var Node:TTreeNode;
begin
 TreeNode.ItemType:=ttnitMemoryReference;
 Node:=TreeNode.Left;
 while assigned(Node) do begin
  OptimizeTree(Node.Left);
  Node:=Node.Right;
 end;
end;

procedure TOptimizerHighLevel.OptimizeFloatConst(var TreeNode:TTreeNode);
begin
// TreeNode.ItemType:=ttnitFPU;
 if not assigned(TreeNode.Return) then begin
  TreeNode.Return:=TypeFLOAT;
 end;
end;

procedure TOptimizerHighLevel.OptimizeType(var TreeNode:TTreeNode);
var OldLeft:TTreeNode;
    OldType:PType;
    CompareTypesEqual:TCompareTypesEqual;
    ConvertType:TConvertType;
    ProcType:PType;
begin
 if assigned(TreeNode.Left) then begin
  OptimizeTree(TreeNode.Left);
 end;
 if assigned(TreeNode.Left) then begin
  CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
  if CompareTypesEqual=tcteIncompatible then begin
   Error.AbortCode(93);
  end else begin
   if CompareTypesEqual=tcteConvertWithPossibleLossOfData then begin
    if not TreeNode.Warning500 then begin
     TreeNode.Warning500:=true;
     Error.AddWarningCode(500);
    end;
   end;
   OldType:=TreeNode.Return;
   OldLeft:=TreeNode.Left;
{  TreeNode.Left:=nil;
   TreeNode.Assign(OldLeft);
   TreeNode.Return:=OldType;
   TreeNode.DoNotOptimize:=true;
   OldLeft.Destroy;
   OptimizeTree(TreeNode);
   exit;}
   TreeNode.Left:=nil;
   FreeAndNil(TreeNode);
   TreeNode:=OldLeft;
   TreeNode.Return:=OldType;
   TreeNode.DoNotOptimize:=true;
   OptimizeTree(TreeNode);
   exit;
  end;
  TreeNode.ItemType:=TreeNode.Left.ItemType;
  TreeNode.ItemValue:=TreeNode.Left.ItemValue;
 end;
end;

procedure TOptimizerHighLevel.OptimizeTypeConv(var TreeNode:TTreeNode);
var OldLeft:TTreeNode;
    OldType:PType;
    CompareTypesEqual:TCompareTypesEqual;
    ConvertType:TConvertType;
    ProcType:PType;
begin
 if assigned(TreeNode.Left) then begin
  OptimizeTree(TreeNode.Left);
 end;
 if not assigned(TreeNode.Return) then begin
  Error.InternalError(200605240938001);
  exit;
 end;
 if assigned(TreeNode.Left) then begin
  if not assigned(TreeNode.Left.Return) then begin
   Error.InternalError(200605240938000);
   exit;
  end;
  if TreeNode.Left.TreeNodeType in [ttntORDConst,ttntCHARConst,ttntSTRINGConst,ttntFloatConst,ttntSETConst,ttntPCHARConst] then begin
   CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
   if CompareTypesEqual=tcteConvertWithPossibleLossOfData then begin
    if not TreeNode.Warning500 then begin
     TreeNode.Warning500:=true;
     Error.AddWarningCode(500);
    end;
   end;
   if CompareTypesEqual=tcteIncompatible then begin
    Error.AbortCode(93);
   end else if CompareTypesEqual=tcteExact then begin
    OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
{   TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft);
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=true;
    OldLeft.Destroy;
    OptimizeTree(TreeNode);
    exit;}
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft;
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=true;
    OptimizeTree(TreeNode);
    exit;
   end else if (ConvertType in [tctIntegerToInteger,tctBooleanToInteger]) and
               (TreeNode.Left.TreeNodeType=ttntORDConst) and
               (TreeNode.Left.Return^.TypeDefinition in [ttdEnumerated,ttdBoolean,ttdSubRange]) and
               (TreeNode.Return^.TypeDefinition in [ttdEnumerated,ttdBoolean,ttdSubRange]) then begin
    case TreeNode.Return^.SubRangeType of
     tstSigned8Bit:begin
      TreeNode.Left.Value:=shortint(byte(byte(shortint(TreeNode.Left.Value)) and $ff));
     end;
     tstSigned16Bit:begin
      TreeNode.Left.Value:=smallint(word(word(smallint(TreeNode.Left.Value)) and $ffff));
     end;
     tstSigned32Bit:begin
      TreeNode.Left.Value:=longint(longword(longword(longint(TreeNode.Left.Value)) and $ffffffff));
     end;
     tstSigned64Bit:begin
      TreeNode.Left.Value:=TreeNode.Left.Value;
     end;
     tstUnsigned8Bit:begin
      TreeNode.Left.Value:=byte(byte(TreeNode.Left.Value) and $ff);
     end;
     tstUnsigned16Bit:begin
      TreeNode.Left.Value:=word(word(TreeNode.Left.Value) and $ffff);
     end;
     tstUnsigned32Bit:begin
      TreeNode.Left.Value:=longword(longword(TreeNode.Left.Value) and $ffffffff);
     end;
     tstUnsigned64Bit:begin
      TreeNode.Left.Value:=TreeNode.Left.Value;
     end;
    end;
    TreeNode.Left.ItemValue:=TreeNode.Left.Value;
{   OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
    TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft);
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=true;
    OldLeft.Destroy;
    OptimizeTree(TreeNode);
    exit;}
    OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft;
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=true;
    OptimizeTree(TreeNode);
    exit;
   end else if (TreeNode.Return^.TypeDefinition=ttdBOOLEAN) and (TreeNode.Left.TreeNodeType in [ttntORDConst]) then begin
    OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
    if TreeNode.Left.Value<>0 then begin
     TreeNode.Left.Value:=TreeNode.Return^.UpperLimit;
    end;
    TreeNode.Left.ItemValue:=TreeNode.Left.Value;
{   TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft);
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=true;
    OldLeft.Destroy;
    OptimizeTree(TreeNode);
    exit;}
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft;
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=true;
    OptimizeTree(TreeNode);
    exit;
   end else if (TreeNode.Left.TreeNodeType=ttntORDConst) and
               (TreeNode.Return^.TypeDefinition=ttdSubRange) and
               (TreeNode.Return^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
    TreeNode.Left.TreeNodeType:=ttntCHARConst;
    TreeNode.Left.CharValue:=TreeNode.Left.Value;
    OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
{  TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft);
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=false;
    OldLeft.Destroy;
    OptimizeTree(TreeNode);
    exit;}
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft;
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=false;
    OptimizeTree(TreeNode);
    exit;
   end else if (TreeNode.Left.TreeNodeType=ttntCHARConst) and
               (TreeNode.Return^.TypeDefinition=ttdSubRange) and not
               (TreeNode.Return^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
    TreeNode.Left.TreeNodeType:=ttntORDConst;
    TreeNode.Left.Value:=ord(TreeNode.Left.CharValue);
    OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
{  TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft);
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=false;
    OldLeft.Destroy;
    OptimizeTree(TreeNode);
    exit;}
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft;
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=false;
    OptimizeTree(TreeNode);
    exit;
   end;
  end else if TreeNode.Left.TreeNodeType=ttntAddress then begin
   if SymbolManager.GetSize(TreeNode.Left.Return)=SymbolManager.GetSize(TreeNode.Return) then begin
    CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
    if CompareTypesEqual=tcteExact then begin
     OldType:=TreeNode.Return;
     OldLeft:=TreeNode.Left;
{   TreeNode.Left:=nil;
     TreeNode.Assign(OldLeft);
     TreeNode.Return:=OldType;
     TreeNode.DoNotOptimize:=true;
     OldLeft.Destroy;
     OptimizeTree(TreeNode);}
     TreeNode.Left:=nil;
     FreeAndNil(TreeNode);
     TreeNode:=OldLeft;
     TreeNode.Return:=OldType;
     TreeNode.DoNotOptimize:=true;
     OptimizeTree(TreeNode);
    end;
   end;
  end else if TreeNode.Left.TreeNodeType=ttntTYPECONV then begin
   CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
   if CompareTypesEqual=tcteExact then begin
    OldType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
{  TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft);
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=false;
    OldLeft.Destroy;
    OptimizeTree(TreeNode);}
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft;
    TreeNode.Return:=OldType;
    TreeNode.DoNotOptimize:=false;
    OptimizeTree(TreeNode);
   end;
  end else if TreeNode.Left.TreeNodeType=ttntVAR then begin
   CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
   if CompareTypesEqual=tcteConvertWithPossibleLossOfData then begin
    if not TreeNode.Warning500 then begin
     TreeNode.Warning500:=true;
     Error.AddWarningCode(500);
    end;
   end;
   if CompareTypesEqual=tcteIncompatible then begin
//  CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
    Error.AbortCode(93);
   end;
{  if SymbolManager.GetSize(TreeNode.Left.Return)=SymbolManager.GetSize(TreeNode.Return) then begin
    if (TreeNode.Left.Return^.TypeDefinition=ttdFloat) and (TreeNode.Return^.TypeDefinition=ttdSubRange) then begin
     Error.AbortCode(93);
    end;
   end else begin
    Error.AbortCode(93);
   end;}
  end else begin
   //if SymbolManager.GetSize(TreeNode.Left.Return)<>SymbolManager.GetSize(TreeNode.Return) then begin
   CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Return,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
   if CompareTypesEqual=tcteConvertWithPossibleLossOfData then begin
    if not TreeNode.Warning500 then begin
     TreeNode.Warning500:=true;
     Error.AddWarningCode(500);
    end;
   end;
   if CompareTypesEqual=tcteIncompatible then begin
    Error.AbortCode(93);
   end;
  end;
  TreeNode.ItemType:=TreeNode.Left.ItemType;
  TreeNode.ItemValue:=TreeNode.Left.ItemValue;
 end;
end;

procedure TOptimizerHighLevel.OptimizeTypeCheck(var TreeNode:TTreeNode);
begin
 if assigned(TreeNode.Left) then begin
  OptimizeTree(TreeNode.Left);
 end;
 if assigned(TreeNode.Left) then begin
  case TreeNode.Left.TreeNodeType of
   ttntVAR,ttntTYPECONV,ttntTYPE:begin
    if (TreeNode.Left.Return=TreeNode.Left.CheckType) or
       SymbolManager.IsObjectClassAncestorType(TreeNode.Left.Return,TreeNode.Left.CheckType) then begin
     FreeAndNil(TreeNode.Left);
     TreeNode.TreeNodeType:=ttntOrdConst;
     TreeNode.Value:=1;
    end;
   end;
  end;
 end;
 TreeNode.Return:=TypeBoolean;
end;

procedure TOptimizerHighLevel.OptimizeVar(var TreeNode:TTreeNode);
var Symbol,TempSymbol,NewSymbol:PSymbol;
    AType:PType;
begin
 case TreeNode.Symbol^.SymbolType of
  Symbols.tstTemp:begin
  end;
  Symbols.tstVariable:begin
   if assigned(TreeNode.Symbol^.LocalProcSymbol) and
      (TreeNode.Symbol^.LocalProcSymbol.LexicalScopeLevel<(SymbolManager.LexicalScopeLevel-1)) then begin
    TreeNode.Symbol^.LocalProcSymbolAccessedFromHigherNestedProc:=true;
   end;
   TreeNode.Symbol^.DeclarationUsed:=true;
   if TreeNode.Symbol^.AbsoluteReference then begin
    TreeNode.ItemType:=ttnitMemory;
   end else begin
    TreeNode.ItemType:=ttnitMemoryReference;
   end;
   TreeNode.Return:=TreeNode.Symbol^.TypeDefinition;
  end;
  Symbols.tstConstant:begin
   if TreeNode.Symbol^.ConstantType=tctOrdinal then begin
    TreeNode.TreeNodeType:=ttntORDConst;
    TreeNode.ItemType:=ttnitOrdinalConstant;
    TreeNode.ItemValue:=TreeNode.Symbol^.IntValue;
    TreeNode.Value:=TreeNode.ItemValue;
    TreeNode.Return:=TreeNode.Symbol^.ConstantTypeRecord;
    if assigned(TreeNode.Left) then begin
     FreeAndNil(TreeNode.Left);
    end;
    if assigned(TreeNode.Right) then begin
     FreeAndNil(TreeNode.Right);
    end;
    exit;
   end;
   TreeNode.Return:=TreeNode.Symbol^.ConstantTypeRecord;
  end;
  Symbols.tstProcedure,Symbols.tstFunction:begin
   TreeNode.ItemType:=ttnitMemoryReference;
   Symbol:=TreeNode.Symbol;
   if not assigned(TreeNode.Return) then begin
    AType:=SymbolManager.NewType(Symbol.OwnerModule);
    AType^.TypeDefinition:=ttdProcedure;
    AType^.ProcedureAttributes:=Symbol^.ProcedureAttributes;
    AType^.MethodPointer:=assigned(Symbol^.OwnerObjectClass);
    AType^.AddressOnly:=false;
    if assigned(Symbol^.Parameter) then begin
     AType^.Parameter:=TSymbolList.Create(SymbolManager);
     TempSymbol:=Symbol^.Parameter.First;
     while assigned(TempSymbol) do begin
      NewSymbol:=SymbolManager.CloneSymbol(TempSymbol);
      AType^.Parameter.AddSymbol(NewSymbol,TempSymbol^.OwnerModule);
      NewSymbol^.LexicalScopeLevel:=TempSymbol^.LexicalScopeLevel;
      TempSymbol:=TempSymbol^.Next;
     end;
    end else begin
     AType^.Parameter:=nil;
    end;
    AType^.ReturnType:=Symbol^.ReturnType;
    TreeNode.Return:=AType;
   end;
  end;
  else begin
   Error.InternalError(201303010614000);
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeAddress(var TreeNode:TTreeNode);
var Symbol,TempSymbol,NewSymbol:PSymbol;
    AType:PType;
begin
 if TreeNode.Left.TreeNodeType=ttntCALL then begin
  // Get function adress AND do not call!
  TreeNode.Left.TreeNodeType:=ttntVAR;
  Symbol:=TreeNode.Left.Symbol;
  AType:=SymbolManager.NewType(Symbol.OwnerModule);
  AType^.TypeDefinition:=ttdProcedure;
  AType^.ProcedureAttributes:=Symbol^.ProcedureAttributes;
  AType^.MethodPointer:=false;
  AType^.AddressOnly:=true;
  if assigned(Symbol^.Parameter) then begin
   AType^.Parameter:=TSymbolList.Create(SymbolManager);
   TempSymbol:=Symbol^.Parameter.First;
   while assigned(TempSymbol) do begin
    NewSymbol:=SymbolManager.CloneSymbol(TempSymbol);
    AType^.Parameter.AddSymbol(NewSymbol,TempSymbol^.OwnerModule);
    NewSymbol^.LexicalScopeLevel:=TempSymbol^.LexicalScopeLevel;
    TempSymbol:=TempSymbol^.Next;
   end;
  end else begin
   AType^.Parameter:=nil;
  end;
  AType^.ReturnType:=Symbol^.ReturnType;
  TreeNode.Left.Return:=AType;
  TreeNode.Return:=AType;
 end;
 OptimizeTree(TreeNode.Left);
 if not assigned(TreeNode.Return) then begin
  if SymbolManager.GlobalSwitches^.TypedAddress and assigned(TreeNode.Left.Return) then begin
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass);
   Symbol^.Name:='_TYPEDADRESS_'+IntToStr(PtrUInt(Symbol));
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.TypeDefinition:=TreeNode.Left.Return;
   TreeNode.Return:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass);
   TreeNode.Return^.TypeDefinition:=ttdPointer;
   TreeNode.Return^.PointerTo:=Symbol;
  end else begin
   Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'POINTER');
   if assigned(Symbol) then begin
    TreeNode.Return:=Symbol^.TypeDefinition;
   end;
  end;
 end;
 if TreeNode.Left.ItemType<>ttnitMemoryReference then begin
  Error.AbortCode(10001);
 end;
end;

procedure TOptimizerHighLevel.OptimizePointer(var TreeNode:TTreeNode);
var OldLeft:TTreeNode;
    NewType:PType;
begin
 OptimizeTree(TreeNode.Left);
 TreeNode.ItemType:=ttnitMemoryReference;
 TreeNode.Left.ItemType:=ttnitMemoryReference;
//TreeNode.Return:=nil;
 if assigned(TreeNode.Left) then begin
  if (TreeNode.Left.TreeNodeType=ttntAddress) and assigned(TreeNode.Left.Left) then begin
   if TreeNode.Left.Left.TreeNodeType=ttntVAR then begin
    NewType:=TreeNode.Return;
    OldLeft:=TreeNode.Left;
{  TreeNode.Left:=nil;
    TreeNode.Assign(OldLeft.Left);
    FreeAndNil(OldLeft);
    TreeNode.Return:=NewType;
    TreeNode.DoNotOptimize:=true;
    exit;}
    TreeNode.Left:=nil;
    FreeAndNil(TreeNode);
    TreeNode:=OldLeft.Left;
    OldLeft.Left:=nil;
    FreeAndNil(OldLeft);
    TreeNode.Return:=NewType;
    TreeNode.DoNotOptimize:=true;
    OptimizeTree(TreeNode);
    exit;
   end else if assigned(TreeNode.Left.Left.Return) then begin
    TreeNode.Return:=TreeNode.Left.Left.Return;
   end;
  end else if assigned(TreeNode.Left.Return) and (TreeNode.Left.Return^.TypeDefinition=ttdPOINTER) and assigned(TreeNode.Left.Return^.PointerTo) then begin
   TreeNode.Return:=TreeNode.Left.Return^.PointerTo^.TypeDefinition;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeField(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 TreeNode.ItemType:=TreeNode.Left.ItemType;
 TreeNode.ItemValue:=TreeNode.Left.ItemValue;
 if not (TreeNode.Left.ItemType in [ttnitMemoryReference,ttnitMemory]) then begin
  Error.AbortCode(10002);
  exit;
 end;
 if not assigned(TreeNode.SymbolField) then begin
  Error.InternalError(200605221840000);
  exit;
 end;
 TreeNode.Return:=TreeNode.SymbolField^.TypeDefinition; // TreeNode.Left.Return;
end;

procedure TOptimizerHighLevel.OptimizeIndex(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 TreeNode.ItemType:=TreeNode.Left.ItemType;
 TreeNode.ItemValue:=TreeNode.Left.ItemValue;
 OptimizeTree(TreeNode.Right);
 if assigned(TreeNode.Left) and assigned(TreeNode.Left.Return) and not (TreeNode.Left.Return^.TypeDefinition in [ttdShortString,ttdLongString]) then begin
  TreeNode.Right:=TreeManager.GenerateTypeConvNode(TreeNode.Right,TypeSigned32Bit,false);
 end;
 if assigned(TreeNode.Right.Left) then begin
  TreeNode.Right.LineNumber:=TreeNode.Right.Left.LineNumber;
 end;
 OptimizeTree(TreeNode.Right);
 TreeNode.Return:=TreeNode.Left.Return^.Definition;
end;

procedure TOptimizerHighLevel.OptimizeAdd(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if (TreeNode.Left.TreeNodeType=ttntCHARConst) and (TreeNode.Right.TreeNodeType=ttntCHARConst) then begin
   TreeNode.TreeNodeType:=ttntSTRINGConst;
   if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedHugeChar) or
      (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedHugeChar) then begin
    TreeNode.Return:=SymbolManager.TypeHugeString;
   end else if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedWideChar) or
               (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedWideChar) then begin
    TreeNode.Return:=SymbolManager.TypeWideString;
   end else begin
    TreeNode.Return:=SymbolManager.TypeAnsiString;
   end;
   TreeNode.StringData:=HugeStringConcat(TreeNode.Left.CharValue,TreeNode.Right.CharValue);
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
  end else if (TreeNode.Left.TreeNodeType=ttntCHARConst) and (TreeNode.Right.TreeNodeType=ttntSTRINGConst) then begin
   TreeNode.TreeNodeType:=ttntSTRINGConst;
   if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedHugeChar) or
      (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedHugeChar) then begin
    TreeNode.Return:=SymbolManager.TypeHugeString;
   end else if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedWideChar) or
               (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedWideChar) then begin
    TreeNode.Return:=SymbolManager.TypeWideString;
   end else begin
    TreeNode.Return:=SymbolManager.TypeAnsiString;
   end;
   TreeNode.StringData:=HugeStringConcat(TreeNode.Left.CharValue,TreeNode.Right.StringData);
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
  end else if (TreeNode.Left.TreeNodeType=ttntSTRINGConst) and (TreeNode.Right.TreeNodeType=ttntCHARConst) then begin
   TreeNode.TreeNodeType:=ttntSTRINGConst;
   if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedHugeChar) or
      (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedHugeChar) then begin
    TreeNode.Return:=SymbolManager.TypeHugeString;
   end else if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedWideChar) or
               (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedWideChar) then begin
    TreeNode.Return:=SymbolManager.TypeWideString;
   end else begin
    TreeNode.Return:=SymbolManager.TypeAnsiString;
   end;
   TreeNode.StringData:=HugeStringConcat(TreeNode.Left.StringData,TreeNode.Right.CharValue);
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
  end else if (TreeNode.Left.TreeNodeType=ttntSTRINGConst) and (TreeNode.Right.TreeNodeType=ttntSTRINGConst) then begin
   TreeNode.TreeNodeType:=ttntSTRINGConst;
   if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedHugeChar) or
      (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedHugeChar) then begin
    TreeNode.Return:=SymbolManager.TypeHugeString;
   end else if (TreeNode.Left.Return.SubRangeType=Symbols.tstUnsignedWideChar) or
               (TreeNode.Right.Return.SubRangeType=Symbols.tstUnsignedWideChar) then begin
    TreeNode.Return:=SymbolManager.TypeWideString;
   end else begin
    TreeNode.Return:=SymbolManager.TypeAnsiString;
   end;
   TreeNode.StringData:=HugeStringConcat(TreeNode.Left.StringData,TreeNode.Right.StringData);
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
  end else begin
   if TreeNode.Left.TreeNodeType=ttntORDConst then begin
    Temp:=TreeNode.Left;
    TreeNode.Left:=TreeNode.Right;
    TreeNode.Right:=Temp;
   end;
   if (TreeNode.Left.TreeNodeType=ttntVAR) and (TreeNode.Right.TreeNodeType=ttntVAR) then begin
    if assigned(TreeNode.Left.Symbol) and assigned(TreeNode.Right.Symbol) then begin
     if (TreeNode.Left.Symbol=TreeNode.Right.Symbol) and
        (TreeNode.Left.Symbol^.TypeDefinition^.TypeDefinition=ttdSubRange) and (TreeNode.Right.Symbol^.TypeDefinition^.TypeDefinition=ttdSubRange) and
        (TreeNode.Left.Symbol^.SymbolType<>Symbols.tstFunction) and(TreeNode.Right.Symbol^.SymbolType<>Symbols.tstFunction) then begin
      TreeNode.TreeNodeType:=ttntMUL;
      TreeNode.Right.TreeNodeType:=ttntORDConst;
      TreeNode.Right.Value:=2;
      exit;
     end;
    end;
   end;
   if assigned(TreeNode.Left.Return) and (TreeNode.Left.Return^.TypeDefinition=ttdSubRange) and
      (TreeNode.Left.Return^.SubRangeType=tstUnsignedChar) and
      assigned(TreeNode.Right.Return) and (TreeNode.Right.Return^.TypeDefinition in [ttdShortString,ttdLongString]) then begin
    TreeNode.Return:=TreeNode.Right.Return;
   end else if assigned(TreeNode.Right.Return) and (TreeNode.Right.Return^.TypeDefinition=ttdSubRange) and
              (TreeNode.Right.Return^.SubRangeType=tstUnsignedChar) and
             assigned(TreeNode.Left.Return) and (TreeNode.Left.Return^.TypeDefinition in [ttdShortString,ttdLongString]) then begin
    TreeNode.Return:=TreeNode.Left.Return;
   end;
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   if not (assigned(TreeNode.Return) or SymbolManager.AssemblerMode) then begin
    if (TreeNode.Left.TreeNodeType in [ttntORDConst,ttntFLOATConst]) then begin
     TreeNode.Return:=TreeNode.Right.Return;
    end else if assigned(TreeNode.Left.Return) and assigned(TreeNode.Right.Return) and
               (TreeNode.Left.Return^.TypeDefinition=ttdSubRange) and (TreeNode.Right.Return^.TypeDefinition=ttdSubRange) then begin
     TreeNode.Return:=TypeSigned32Bit;
    end else begin
     TreeNode.Return:=TreeNode.Left.Return;
    end;
   end;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeSub(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeOr(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   Temp:=TreeNode.Left;
   TreeNode.Left:=TreeNode.Right;
   TreeNode.Right:=Temp;
  end;
  MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
  if not assigned(TreeNode.Return) then TreeNode.Return:=TreeNode.Left.Return;
 end;
end;

procedure TOptimizerHighLevel.OptimizeXor(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   Temp:=TreeNode.Left;
   TreeNode.Left:=TreeNode.Right;
   TreeNode.Right:=Temp;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeShl(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeShr(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeCompare(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
     Error.AbortCode(7);
    end;
   end;
  end;
  if TreeNode.Left.Return^.TypeDefinition=ttdSubRange then begin
   TreeNode.Signed:=TreeNode.Left.Return^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit];
  end;
  if not assigned(TreeNode.CompareType) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.CompareType:=TreeNode.Left.Return;
  end;
  TreeNode.Return:=TypeBoolean;
 end;
end;

procedure TOptimizerHighLevel.OptimizeWITH(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
end;

procedure TOptimizerHighLevel.OptimizeAnd(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   Temp:=TreeNode.Left;
   TreeNode.Left:=TreeNode.Right;
   TreeNode.Right:=Temp;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeIn(var TreeNode:TTreeNode);
//var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
{if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   Temp:=TreeNode.Left;
   TreeNode.Left:=TreeNode.Right;
   TreeNode.Right:=Temp;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;}
end;

procedure TOptimizerHighLevel.OptimizeMul(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   Temp:=TreeNode.Left;
   TreeNode.Left:=TreeNode.Right;
   TreeNode.Right:=Temp;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeDiv(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeMod(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeSlash(var TreeNode:TTreeNode);
var FloatValue:extended;
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 if not PrecalculateConstants(TreeNode) then begin
  if TreeNode.Left.TreeNodeType=ttntORDConst then begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
    Error.AbortCode(7);
   end;
  end else begin
   if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Return,TreeNode.Right.Return) then begin
    Error.AbortCode(7);
   end;
  end;
  case TreeNode.Right.TreeNodeType of
   ttntORDConst:begin
    TreeNode.TreeNodeType:=ttntMUL;
    FloatValue:=1/TreeNode.Right.Value;
    FreeAndNil(TreeNode.Right);
    TreeNode.Right:=TreeManager.GenerateFloatConstNode(FloatValue,TypeFloat);
   end;
   ttntFLOATConst:begin
    TreeNode.TreeNodeType:=ttntMUL;
    FloatValue:=1/TreeNode.Right.FloatValue;
    FreeAndNil(TreeNode.Right);
    TreeNode.Right:=TreeManager.GenerateFloatConstNode(FloatValue,TypeFloat);
   end;
  end;
  if not assigned(TreeNode.Return) then begin
   MaybeTypeConversion(TreeNode.Left,TreeNode.Right);
   TreeNode.Return:=TypeFloat;
   if TreeNode.Left.Return^.TypeDefinition<>ttdFloat then begin
    TreeNode.Left:=TreeManager.GenerateTypeConvNode(TreeNode.Left,TypeFloat,false);
   end;
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) then begin
    TreeNode.Left.LineNumber:=TreeNode.Left.Left.LineNumber;
   end;
   OptimizeTree(TreeNode.Left);
   if TreeNode.Right.Return^.TypeDefinition<>ttdFloat then begin
    TreeNode.Right:=TreeManager.GenerateTypeConvNode(TreeNode.Left,TypeFloat,false);
   end;
   if assigned(TreeNode.Right) and assigned(TreeNode.Right.Left) then begin
    TreeNode.Right.LineNumber:=TreeNode.Right.Left.LineNumber;
   end;
   OptimizeTree(TreeNode.Right);
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeMinus(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 TreeNode.Return:=TreeNode.Left.Return;
 case TreeNode.Left.TreeNodeType of
  ttntORDConst:begin
   TreeNode.TreeNodeType:=ttntORDConst;
   TreeNode.Value:=-TreeNode.Left.Value;
   FreeAndNil(TreeNode.Left);
  end;
  ttntFLOATConst:begin
   TreeNode.TreeNodeType:=ttntFLOATConst;
   TreeNode.FloatValue:=-TreeNode.Left.FloatValue;
   FreeAndNil(TreeNode.Left);
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeNot(var TreeNode:TTreeNode);
var OldNode:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 case TreeNode.Left.TreeNodeType of
  ttntLess:begin
   OldNode:=TreeNode.Left;
   TreeNode.TreeNodeType:=ttntGreaterOrEqual;
   TreeNode.Return:=TreeNode.Left.Return;
   TreeNode.Left:=OldNode.Left;
   TreeNode.Right:=OldNode.Right;
   OldNode.Left:=nil;
   OldNode.Right:=nil;
   FreeAndNil(OldNode);
  end;
  ttntLessOrEqual:begin
   OldNode:=TreeNode.Left;
   TreeNode.TreeNodeType:=ttntGreater;
   TreeNode.Return:=TreeNode.Left.Return;
   TreeNode.Left:=OldNode.Left;
   TreeNode.Right:=OldNode.Right;
   OldNode.Left:=nil;
   OldNode.Right:=nil;
   FreeAndNil(OldNode);
  end;
  ttntGreater:begin
   OldNode:=TreeNode.Left;
   TreeNode.TreeNodeType:=ttntLessOrEqual;
   TreeNode.Return:=TreeNode.Left.Return;
   TreeNode.Left:=OldNode.Left;
   TreeNode.Right:=OldNode.Right;
   OldNode.Left:=nil;
   OldNode.Right:=nil;
   FreeAndNil(OldNode);
  end;
  ttntGreaterOrEqual:begin
   OldNode:=TreeNode.Left;
   TreeNode.TreeNodeType:=ttntLess;
   TreeNode.Return:=TreeNode.Left.Return;
   TreeNode.Left:=OldNode.Left;
   TreeNode.Right:=OldNode.Right;
   OldNode.Left:=nil;
   OldNode.Right:=nil;
   FreeAndNil(OldNode);
  end;
  ttntEqual:begin
   OldNode:=TreeNode.Left;
   TreeNode.TreeNodeType:=ttntNotEqual;
   TreeNode.Return:=TreeNode.Left.Return;
   TreeNode.Left:=OldNode.Left;
   TreeNode.Right:=OldNode.Right;
   OldNode.Left:=nil;
   OldNode.Right:=nil;
   FreeAndNil(OldNode);
  end;
  ttntNotEqual:begin
   OldNode:=TreeNode.Left;
   TreeNode.TreeNodeType:=ttntEqual;
   TreeNode.Return:=TreeNode.Left.Return;
   TreeNode.Left:=OldNode.Left;
   TreeNode.Right:=OldNode.Right;
   OldNode.Left:=nil;
   OldNode.Right:=nil;
   FreeAndNil(OldNode);
  end;
 end;
 if TreeNode.Left.TreeNodeType=ttntORDConst then begin
  TreeNode.TreeNodeType:=ttntORDConst;
  if assigned(TreeNode.Left.Return) and (TreeNode.Left.Return^.TypeDefinition=ttdBoolean) then begin
   TreeNode.Value:=TreeNode.Left.Value xor 1;
  end else begin
   TreeNode.Value:=not TreeNode.Left.Value;
  end;
  FreeAndNil(TreeNode.Left);
 end;
 if not assigned(TreeNode.Return) then begin
  TreeNode.Return:=TreeNode.Left.Return;
 end;
end;

procedure TOptimizerHighLevel.OptimizeSubRange(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
end;

procedure TOptimizerHighLevel.OptimizeParameter(var TreeNode:TTreeNode;Symbol:PSymbol);
var CompareTypesEqual:TCompareTypesEqual;
    ConvertType:TConvertType;
    ProcType:PType;
begin
 OptimizeTree(TreeNode.Left);
 if assigned(Symbol) then begin
  TreeNode.ParameterSymbol:=Symbol;
  if assigned(TreeNode.Left) then begin
   TreeNode.Left.ParameterSymbol:=Symbol;
  end;
  if LocalSwitches^.VarStringChecks then begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Return) and assigned(Symbol^.TypeDefinition) then begin
    if (TreeNode.Left.Return^.TypeDefinition=ttdSHORTSTRING) and
       (Symbol^.TypeDefinition^.TypeDefinition=ttdSHORTSTRING) and
       (TreeNode.Left.Return^.Length<>Symbol^.TypeDefinition^.Length) then begin
     Error.AddErrorCode(7);
    end;
   end;
  end;
  if (Symbol^.VariableType in [tvtParameterVariable,tvtParameterResult]) or
     ((Symbol^.VariableType in [tvtParameterConstant]) and (Symbol^.TypeDefinition^.TypeDefinition=ttdEmpty)) then begin
   if not TreeNode.Left.DoNotOptimize then begin
    OptimizeTree(TreeNode.Left);
    if Symbol^.TypeDefinition^.TypeDefinition=ttdEmpty then begin
     TreeNode.ReferenceParameter:=true;
{
//   TreeNode.Left.ItemType:=ttnitMemoryReference;
     TreeNode.Left:=TreeManager.GenerateLeftNode(ttntAddress,TreeNode.Left);
     OptimizeTree(TreeNode.Left);
     TreeNode.Return:=TreeNode.Left.Return;
     TreeNode.Left.DoNotOptimize:=true;     }
    end else begin
     CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,Symbol^.TypeDefinition,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
     case CompareTypesEqual of
      tcteIncompatible:begin
       Error.AddErrorCode(7);
      end;
      tcteEqual:begin
       TreeNode.Left:=TreeManager.GenerateTypeConvNode(TreeNode.Left,Symbol^.TypeDefinition,false);
      end;
      else begin
       TreeNode.Left.Return:=Symbol^.TypeDefinition;
      end;
     end;
     TreeNode.Return:=TreeNode.Left.Return;
     TreeNode.ReferenceParameter:=true;
{
//   TreeNode.Left.ItemType:=ttnitMemoryReference;
     TreeNode.Left:=TreeManager.GenerateLeftNode(ttntAddress,TreeNode.Left);
     OptimizeTree(TreeNode.Left);
     TreeNode.Return:=TreeNode.Left.Return;
     TreeNode.Left.DoNotOptimize:=true;}
    end;
   end;
  end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Return) and assigned(Symbol^.TypeDefinition) and
     ((TreeNode.Left.Return^.TypeDefinition<>Symbol^.TypeDefinition^.TypeDefinition) or
      (SymbolManager.GetSize(TreeNode.Left.Return)<>SymbolManager.GetSize(Symbol^.TypeDefinition))) then begin
   TreeNode.Left:=TreeManager.GenerateTypeConvNode(TreeNode.Left,Symbol^.TypeDefinition,false);
  end else begin
   CompareTypesEqual:=CompareTypesExt(Error,SymbolManager,TreeNode.Left.Return,Symbol^.TypeDefinition,TreeNode.TreeNodeType,ConvertType,ProcType,[tctoEXPLICIT]);
   case CompareTypesEqual of
    tcteIncompatible:begin
     Error.AddErrorCode(7);
    end;
    tcteConvertOperator,
    tcteConvertWithPossibleLossOfData,
    tcteConvertWithLessPreferedConversion,
    tcteConvertCompatible,
    tcteEqual:begin
     TreeNode.Left:=TreeManager.GenerateTypeConvNode(TreeNode.Left,Symbol^.TypeDefinition,false);
    end;
    else begin
     TreeNode.Left.Return:=Symbol^.TypeDefinition;
    end;
   end;
   TreeNode.Return:=TreeNode.Left.Return;
  end;
 end;
 if assigned(Symbol) then begin
  if assigned(Symbol^.Next) then begin
   Symbol:=Symbol^.Next;
   if assigned(TreeNode.Right) then begin
    OptimizeParameter(TreeNode.Right,Symbol);
   end;
   OptimizeTree(TreeNode.Left);
  end;
 end else if assigned(TreeNode.Right) then begin
  OptimizeParameter(TreeNode.Right,Symbol);
 end;
end;

procedure TOptimizerHighLevel.OptimizeInternalRoutines(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 case TreeNode.Symbol^.InternalProcedure of
  tipWRITE,tipWRITELN,tipREAD,tipREADLN:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Symbol^.Parameter) then begin
    OptimizeParameter(TreeNode.Left,TreeNode.Symbol^.Parameter.First);
   end;
  end;
  tipDEC,tipINC:begin
  end;
  tipSUCC,tipPRED:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    OptimizeTree(TreeNode.Left.Left);
    TreeNode.Return:=TreeNode.Left.Left.Return;
    if TreeNode.Left.Left.TreeNodeType=ttntORDConst then begin
     TreeNode.TreeNodeType:=ttntORDConst;
     TreeNode.ItemType:=ttnitOrdinalConstant;
     case TreeNode.Symbol^.InternalProcedure of
      tipSUCC:begin
       TreeNode.ItemValue:=succ(TreeNode.Left.Left.Value);
      end;
      tipPRED:begin
       TreeNode.ItemValue:=pred(TreeNode.Left.Left.Value);
      end;
     end;
     TreeNode.Value:=TreeNode.ItemValue;
     FreeAndNil(TreeNode.Left);
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipORD:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    OptimizeTree(TreeNode.Left.Left);
    if TreeNode.Left.Left.TreeNodeType=ttntORDConst then begin
     TreeNode.TreeNodeType:=ttntORDConst;
     TreeNode.ItemType:=ttnitOrdinalConstant;
     TreeNode.ItemValue:=TreeNode.Left.Left.Value;
     TreeNode.Value:=TreeNode.ItemValue;
     FreeAndNil(TreeNode.Left);
    end else begin
     TreeNode.Return:=TypeSigned32Bit;
     TreeNode:=TreeManager.GenerateTypeConvNode(TreeNode.Left.Left,TypeSigned32Bit,true);
     TreeNode.LineNumber:=TreeNode.Left.LineNumber;
     OptimizeTree(TreeNode);
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipCHR:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    Temp:=TreeNode.Left.Left;
    OptimizeTree(Temp);
    if Temp.TreeNodeType=ttntORDConst then begin
     TreeNode.TreeNodeType:=ttntCharConst;
     TreeNode.ItemType:=ttnitOrdinalConstant;
     TreeNode.ItemValue:=Temp.Value;
     TreeNode.Value:=TreeNode.ItemValue;
     FreeAndNil(TreeNode.Left);
    end else begin
     TreeNode.Return:=TypeChar;
{    TreeNode.Return:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass);
     TreeNode.Return^.TypeDefinition:=ttdSubRange;
     TreeNode.Return^.SubRangeType:=tstUnsignedChar;}
     TreeNode:=TreeManager.GenerateTypeConvNode(Temp,TreeNode.Return,true);
     TreeNode.LineNumber:=TreeNode.Left.LineNumber;
     OptimizeTree(TreeNode);
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipSIZEOF:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    TreeNode.TreeNodeType:=ttntORDConst;
    TreeNode.ItemType:=ttnitOrdinalConstant;
    TreeNode.ItemValue:=SymbolManager.GetSize(TreeNode.Left.Left.Return);
    TreeNode.Value:=TreeNode.ItemValue;
    FreeAndNil(TreeNode.Left);
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipSETLENGTH:begin
  end;
  tipLENGTH:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    Temp:=TreeNode.Left.Left;
    OptimizeTree(Temp);
    TreeNode.Symbol.ReturnType:=TypeSigned32Bit;
    TreeNode.Return:=TypeSigned32Bit;
    case Temp.TreeNodeType of
     ttntSTRINGConst:begin
      TreeNode.TreeNodeType:=ttntORDConst;
      TreeNode.ItemType:=ttnitOrdinalConstant;
      TreeNode.ItemValue:=length(Temp.StringData);
      TreeNode.Value:=TreeNode.ItemValue;
      FreeAndNil(TreeNode.Left);
     end;
     ttntCHARConst:begin
      TreeNode.TreeNodeType:=ttntORDConst;
      TreeNode.ItemType:=ttnitOrdinalConstant;
      TreeNode.ItemValue:=1;
      TreeNode.Value:=TreeNode.ItemValue;
      FreeAndNil(TreeNode.Left);
     end;
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipASSIGNED:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    OptimizeTree(TreeNode.Left.Left);
//  TreeNode.Symbol.ReturnType:=TypeBoolean;
    TreeNode.Return:=TypeBoolean;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipNEW:begin
  end;
  tipDISPOSE:begin
  end;
  tipTRUNC:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    case TreeNode.Left.Left.Return^.TypeDefinition of
     ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
      TreeNode.Return:=TreeNode.Left.Left.Return;
      if TreeNode.Left.Left.TreeNodeType=ttntORDConst then begin
       TreeNode.TreeNodeType:=ttntORDConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.ItemValue:=TreeNode.Left.Left.ItemValue;
       TreeNode.Value:=TreeNode.Left.Left.Value;
       FreeAndNil(TreeNode.Left);
      end;
     end;
     ttdFloat:begin
      case TreeNode.Left.Left.Return^.FloatType of
       tstFloat32Bit:begin
        TreeNode.Return:=TypeSigned32Bit;
       end;
       tstFloat64Bit:begin
        TreeNode.Return:=TypeSigned64Bit;
       end;
       else {tstFloat80Bit:}begin
        TreeNode.Return:=TypeSigned64Bit;
       end;
      end;
      if TreeNode.Left.Left.TreeNodeType=ttntFloatConst then begin
       TreeNode.TreeNodeType:=ttntFloatConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.ItemValue:=trunc(TreeNode.Left.Left.FloatValue);
       TreeNode.Value:=trunc(TreeNode.Left.Left.FloatValue);
       FreeAndNil(TreeNode.Left);
      end;
     end;
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipROUND:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    case TreeNode.Left.Left.Return^.TypeDefinition of
     ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
      TreeNode.Return:=TreeNode.Left.Left.Return;
      if TreeNode.Left.Left.TreeNodeType=ttntORDConst then begin
       TreeNode.TreeNodeType:=ttntORDConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.ItemValue:=TreeNode.Left.Left.ItemValue;
       TreeNode.Value:=TreeNode.Left.Left.Value;
       FreeAndNil(TreeNode.Left);
      end;
     end;
     ttdFloat:begin
      case TreeNode.Left.Left.Return^.FloatType of
       tstFloat32Bit:begin
        TreeNode.Return:=TypeSigned32Bit;
       end;
       tstFloat64Bit:begin
        TreeNode.Return:=TypeSigned64Bit;
       end;
       else {tstFloat80Bit:}begin
        TreeNode.Return:=TypeSigned64Bit;
       end;
      end;
      if TreeNode.Left.Left.TreeNodeType=ttntFloatConst then begin
       TreeNode.TreeNodeType:=ttntFloatConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.ItemValue:=round(TreeNode.Left.Left.FloatValue);
       TreeNode.Value:=round(TreeNode.Left.Left.FloatValue);
       FreeAndNil(TreeNode.Left);
      end;
     end;
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipSQR:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    case TreeNode.Left.Left.Return^.TypeDefinition of
     ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
      TreeNode.Return:=TreeNode.Left.Left.Return;
      if TreeNode.Left.Left.TreeNodeType=ttntORDConst then begin
       TreeNode.TreeNodeType:=ttntORDConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.ItemValue:=sqr(TreeNode.Left.Left.ItemValue);
       TreeNode.Value:=sqr(TreeNode.Left.Left.Value);
       FreeAndNil(TreeNode.Left);
      end;
     end;
     ttdFloat:begin
      TreeNode.Return:=TreeNode.Left.Left.Return;
      if TreeNode.Left.Left.TreeNodeType=ttntFloatConst then begin
       TreeNode.TreeNodeType:=ttntFloatConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.FloatValue:=sqr(TreeNode.Left.Left.FloatValue);
       FreeAndNil(TreeNode.Left);
      end;
     end;
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipSQRT:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
    case TreeNode.Left.Left.Return^.TypeDefinition of
     ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
      TreeNode.Return:=TypeFloat;
      if TreeNode.Left.Left.TreeNodeType=ttntORDConst then begin
       TreeNode.TreeNodeType:=ttntFloatConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.FloatValue:=sqrt(TreeNode.Left.Left.Value+0.0);
       FreeAndNil(TreeNode.Left);
      end;
     end;
     ttdFloat:begin
      TreeNode.Return:=TreeNode.Left.Left.Return;
      if TreeNode.Left.Left.TreeNodeType=ttntFloatConst then begin
       TreeNode.TreeNodeType:=ttntFloatConst;
       TreeNode.ItemType:=ttnitOrdinalConstant;
       TreeNode.FloatValue:=sqrt(TreeNode.Left.Left.FloatValue);
       FreeAndNil(TreeNode.Left);
      end;
     end;
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipTYPEOF:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
    TreeNode.Return:=TypePointer;
    if not ((TreeNode.Left.Left.Return^.TypeDefinition=ttdOBJECT) and TreeNode.Left.Left.Return^.HasVirtualTable) then begin
     Error.AddErrorCode(86);
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipTYPEINFO:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
    TreeNode.Return:=TypePointer;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipINITIALIZE:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
    if not TreeNode.Left.Left.Return^.NeedTypeInfo then begin
     if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
      Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
     end else begin
      Error.AbortCode(138,'???');
     end;
    end;
   end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and assigned(TreeNode.Left.Right.Left.Return) and not assigned(TreeNode.Left.Right.Right) then begin
    if not TreeNode.Left.Left.Return^.NeedTypeInfo then begin
     if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
      Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
     end else begin
      Error.AbortCode(138,'???');
     end;
    end else if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Right.Left.Return,TypeSigned32Bit) then begin
     Error.AbortCode(7);
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  tipFINALIZE:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
    if not TreeNode.Left.Left.Return^.NeedTypeInfo then begin
     if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
      Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
     end else begin
      Error.AbortCode(138,'???');
     end;
    end;
   end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and assigned(TreeNode.Left.Right.Left.Return) and not assigned(TreeNode.Left.Right.Right) then begin
    if not TreeNode.Left.Left.Return^.NeedTypeInfo then begin
     if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
      Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
     end else begin
      Error.AbortCode(138,'???');
     end;
    end else if not AreTypesCompatible(Error,SymbolManager,TreeNode.Left.Right.Left.Return,TypeSigned32Bit) then begin
     Error.AbortCode(7);
    end;
   end else begin
    Error.AbortCode(10004);
   end;
  end;
  else begin
   Error.InternalError(200605180941000);
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeCall(var TreeNode:TTreeNode);
var Symbol:PSymbol;
begin
 if assigned(TreeNode.Right) then begin
  OptimizeTree(TreeNode.Right);
 end;
 if assigned(TreeNode.MethodSymbol) then begin
  Symbol:=TreeNode.MethodSymbol;
 end else begin
  Symbol:=TreeNode.Symbol;
 end;
 if not assigned(Symbol) then begin
  Error.InternalError(200605180941001);
  exit;
 end;
 if (Symbol^.SymbolType=Symbols.tstVariable) and assigned(Symbol^.TypeDefinition) and
    (Symbol^.TypeDefinition^.TypeDefinition=ttdProcedure) then begin
  if assigned(TreeNode.Left) and assigned(Symbol^.TypeDefinition^.Parameter) then begin
   OptimizeParameter(TreeNode.Left,Symbol^.TypeDefinition^.Parameter.First);
  end;
  TreeNode.Return:=Symbol^.TypeDefinition;
 end else begin
  if assigned(TreeNode.Left) and assigned(Symbol) and assigned(Symbol^.Parameter) then begin
   OptimizeParameter(TreeNode.Left,Symbol^.Parameter.First);
  end;
 end;
 if Symbol^.InternalProcedure<>tipNone then begin
  OptimizeInternalRoutines(TreeNode);
 end else begin
  if not assigned(TreeNode.Return) then begin
   TreeNode.Return:=Symbol^.ReturnType;
  end;
  if Symbol.SymbolType=Symbols.tstFunction then begin
   case TreeNode.Return^.TypeDefinition of
    ttdBoolean,ttdPointer,ttdSubRange,ttdFloat:begin
     TreeNode.ItemType:=ttnitTemporary;
    end;
    else begin
     TreeNode.ItemType:=ttnitMemoryReference;
    end;
   end;
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeRESULT(var TreeNode:TTreeNode);
begin
 TreeNode.Return:=TreeNode.Symbol^.ReturnType;
 TreeNode.ItemType:=ttnitMemoryReference;
end;

procedure TOptimizerHighLevel.OptimizeAssign(var TreeNode:TTreeNode);
var TreeNodeEx:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 if not assigned(TreeNode.Left) then begin
  Error.AbortCode(37);
  exit;
 end else if not (TreeNode.Left.TreeNodeType in [ttntINDEX,ttntVAR,ttntCALL,ttntFIELD]) then begin
//end else if TreeNode.Left.TreeNodeType IN [ttntORDConst,ttntFLOATConst,ttntCHARConst,ttntStringConst,ttntWideStringConst] then begin
  Error.AbortCode(37);
  exit;
 end;
 TreeNodeEx:=TreeNode.Left;
 while TreeNodeEx.TreeNodeType=ttntINDEX do begin
  TreeNodeEx:=TreeNodeEx.Left;
 end;
 if (TreeNodeEx.TreeNodeType=ttntVAR) and assigned(TreeNodeEx.Symbol) and
    (tsaFORControlVariable in TreeNodeEx.Symbol^.Attributes) then begin
  Error.AbortCode(84,CorrectSymbolName(TreeNodeEx.Symbol^.name));
  exit;
 end;
 OptimizeTree(TreeNode.Right);
 if (TreeNode.Left.TreeNodeType=ttntCALL) and
    (TreeNode.Right.TreeNodeType=ttntCALL) and
    assigned(TreeNode.Left.Return) and
    assigned(TreeNode.Right.Symbol) and
    (TreeNode.Left.Return^.TypeDefinition=ttdProcedure) then begin
  case CompareProcToProcVar(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) of
   tcteIncompatible:begin
    Error.AbortCode(7);
   end;
  end;
  TreeNode.Left.TreeNodeType:=ttntVar;
  TreeNode.Right:=TreeManager.GenerateLeftNode(ttntAddress,TreeNode.Right);
  OptimizeTree(TreeNode.Right);
  TreeNode.Right.Return:=TreeNode.Left.Return;
 end else if TreeNode.Left.TreeNodeType=ttntCALL then begin
  TreeNode.Left.TreeNodeType:=ttntVAR;
  TreeNode.Left.Return:=TreeNode.Left.Symbol^.ReturnType;
  TreeNode.Left.Symbol:=TreeNode.Left.Symbol^.ResultSymbol;
  TreeNode.Left.ItemType:=ttnitMemoryReference;
 end;
 if (TreeNode.Right.TreeNodeType in [ttntSTRINGConst,ttntCHARConst]) and
    assigned(TreeNode.Left.Return) and (TreeNode.Left.Return^.TypeDefinition=ttdPointer) and
    assigned(TreeNode.Left.Return^.PointerTo) and (TreeNode.Left.Return^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
    (TreeNode.Left.Return^.PointerTo^.TypeDefinition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
  TreeNode.Right.TreeNodeType:=ttntPCHARconst;
  TreeNode.Right.Return:=TreeNode.Left.Return;
 end;
 if (not (TreeNode.Right.TreeNodeType in [ttntSTRINGConst,ttntCHARConst])) and
    assigned(TreeNode.Left.Return) and assigned(TreeNode.Right.Return) and
    ((TreeNode.Left.Return^.SubRangeType<>TreeNode.Right.Return^.SubRangeType) or
     (TreeNode.Left.Return^.TypeDefinition<>TreeNode.Right.Return^.TypeDefinition) or
     (SymbolManager.GetSize(TreeNode.Left.Return)<>SymbolManager.GetSize(TreeNode.Right.Return))) then begin
  if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Left.Return) then begin
   Error.AbortCode(7);
  end;
  TreeNode.Right:=TreeManager.GenerateTypeConvNode(TreeNode.Right,TreeNode.Left.Return,false);
  TreeNode.Right.LineNumber:=TreeNode.Right.Left.LineNumber;
  OptimizeTree(TreeNode.Right);
 end;
 if not assigned(TreeNode.Return) then begin
  TreeNode.Return:=TreeNode.Left.Return;
 end;
 if not AreTypesCompatible(Error,SymbolManager,TreeNode.Right.Return,TreeNode.Return) then begin
  Error.AbortCode(7);
 end;
 if assigned(TreeNode.Left) and (TreeNode.Left.TreeNodeType=ttntVAR) and assigned(TreeNode.Left.Symbol) and (TreeNode.Left.Symbol^.VariableType=tvtParameterConstant) then begin
  Error.AddErrorCode(65);
 end;
end;

procedure TOptimizerHighLevel.OptimizeBlock(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 if not assigned(TreeNode.Left) then begin
  if assigned(TreeNode.Right) then begin
   FreeAndNil(TreeNode.Right);
  end;
  TreeNode.TreeNodeType:=ttntEMPTY;
  exit;
 end;
 if not assigned(TreeNode.Left.Left) then begin
  Temp:=TreeNode;
  TreeNode:=TreeNode.Left;
  Temp.Left:=nil;
  FreeAndNil(Temp);
{ Temp:=TreeNode.Left;
  TreeNode.Left:=nil;
  TreeNode.Assign(Temp);
  FreeAndNil(Temp);}
  exit;
 end;
 Temp:=TreeNode.Left;
 while assigned(Temp) do begin
  OptimizeTree(Temp.Right);
  Temp:=Temp.Left;
 end;
end;

procedure TOptimizerHighLevel.OptimizeFOR(var TreeNode:TTreeNode);
var Temp:TTreeNode;
    IsDeadCode,IsORDLeft,IsORDRight:boolean;
    ORDLeftValue,ORDRightValue:int64;
begin
 OptimizeTree(TreeNode.Left);
 if assigned(TreeNode.Right) and
    (TreeNode.Right.TreeNodeType in [ttntGreater,ttntLess,ttntGreaterOrEqual,ttntLessOrEqual]) then begin
  exit;
 end;
 if assigned(TreeNode.Left) and (TreeNode.Left.TreeNodeType=ttntAssign) and (((TreeNode.Left.Left.TreeNodeType<>ttntVAR) or (false)) or not assigned(TreeNode.Left.Left)) then begin
  Error.AbortCode(32);
  exit;
 end else if assigned(TreeNode.Left) and (TreeNode.Left.TreeNodeType<>ttntAssign) then begin
  Error.AbortCode(32);
  exit;
 end else if not assigned(TreeNode.Left) then begin
  Error.AbortCode(32);
  exit;
 end else if (assigned(TreeNode.Left.Return) and not (TreeNode.Left.Return.TypeDefinition in [ttdSubRange,ttdEnumerated])) or not assigned(TreeNode.Left.Return) then begin
  Error.AbortCode(33);
  exit;
 end;

 OptimizeTree(TreeNode.Right);
 if LocalSwitches^.Optimization then begin
  if assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Right) and (TreeNode.Left.TreeNodeType=ttntAssign) and not (assigned(TreeNode.Block) and TreeNode.Block.CheckForNodes([ttntLABEL,ttntASM,ttntCCODE,ttntCEXPRESSION])) then begin
   IsORDLeft:=false;
   IsORDRight:=false;
   ORDLeftValue:=0;
   ORDRightValue:=0;
   if TreeNode.Left.Right.TreeNodeType=ttntORDCONST then begin
    IsORDLeft:=true;
    ORDLeftValue:=TreeNode.Left.Right.Value;
   end else if (TreeNode.Left.Right.TreeNodeType=ttntTYPECONV) and assigned(TreeNode.Left.Right.Left) and (TreeNode.Left.Right.Left.TreeNodeType=ttntORDCONST) then begin
    IsORDLeft:=true;
    ORDLeftValue:=TreeNode.Left.Right.Left.Value;
   end;
   if TreeNode.Right.TreeNodeType=ttntORDCONST then begin
    IsORDRight:=true;
    ORDRightValue:=TreeNode.Right.Value;
   end else if (TreeNode.Right.TreeNodeType=ttntTYPECONV) and assigned(TreeNode.Right.Left) and (TreeNode.Right.Left.TreeNodeType=ttntORDCONST) then begin
    IsORDRight:=true;
    ORDRightValue:=TreeNode.Right.Left.Value;
   end;
   if IsORDLeft and IsORDRight then begin
    if TreeNode.IsDownTo then begin
     IsDeadCode:=ORDLeftValue<ORDRightValue;
    end else begin
     IsDeadCode:=ORDLeftValue>ORDRightValue;
    end;
    if IsDeadCode then begin
     // Dead code found - Kill it :-)
     Error.AddHintCode(139);
     TreeNode.TreeNodeType:=ttntEMPTY;
     FreeAndNil(TreeNode.Left);
     FreeAndNil(TreeNode.Right);
     FreeAndNil(TreeNode.Block);
     exit;
    end else begin
     OptimizeTree(TreeNode.Block);
     if ORDLeftValue=ORDRightValue then begin
      if assigned(TreeNode.Block) then begin
       IsDeadCode:=TreeNode.Block.TreeNodeType=ttntEMPTY;
      end else begin
       IsDeadCode:=true;
      end;
      if IsDeadCode then begin
       // Special dead code found -> compact it -> convert "FOR X:=123 TO 123 DO;" to "X:=123;"
       FreeAndNil(TreeNode.Right);
       FreeAndNil(TreeNode.Block);
       Temp:=TreeNode.Left;
       TreeNode.Left:=nil;
       TreeNode.TreeNodeType:=Temp.TreeNodeType;
       TreeNode.Left:=Temp.Left;
       TreeNode.Right:=Temp.Right;
       Temp.Left:=nil;
       Temp.Right:=nil;
       FreeAndNil(Temp);
       OptimizeTree(TreeNode);
       exit;
      end else if not TreeNode.Block.CheckForNodes([ttntCONTINUE,ttntBREAK,ttntCCODE,ttntCEXPRESSION]) then begin
       // Special dead code found -> compact it -> convert "FOR X:=123 TO 123 DO XYZ;" to "X:=123; XYZ;"
       TreeNode.TreeNodeType:=ttntBlock;
       TreeNode.Left:=TreeManager.GenerateLeftRightNode(ttntSTATEMENT,TreeManager.GenerateLeftRightNode(ttntSTATEMENT,nil,TreeNode.Block),TreeNode.Left);
       FreeAndNil(TreeNode.Right);
       TreeNode.Right:=nil;
       TreeNode.Block:=nil;
       OptimizeTree(TreeNode);
       exit;
      end;
     end else if (TreeNode.Left.TreeNodeType=ttntAssign) and not assigned(TreeNode.Block) then begin
      TreeNode.TreeNodeType:=ttntBlock;
      Temp:=TreeNode.Right;
      FreeAndNil(TreeNode.Left.Right);
      TreeNode.Left.Right:=Temp;
      TreeNode.Left:=TreeManager.GenerateLeftRightNode(ttntSTATEMENT,nil,TreeNode.Left);
      TreeNode.Right:=nil;
      TreeNode.Block:=nil;
      OptimizeTree(TreeNode);
      exit;
     end else begin
      // TODO: Optional unroll static FOP loops
     end;
    end;
   end;
  end;
 end;

 // Convert "FOR A:=X TO Y DO" to "(FOR) A:=X WHILE A<=X DO"
 Temp:=TTreeNode.CreateFrom(TreeNode.Left);
 FreeAndNil(Temp.Right);
 Temp.Right:=TreeNode.Right;
 if TreeNode.IsDownTo then begin
  Temp.TreeNodeType:=ttntGreaterOrEqual;
 end else begin
  Temp.TreeNodeType:=ttntLessOrEqual;
 end;
 TreeNode.Right:=Temp;

 OptimizeTree(TreeNode.Right);
 OptimizeTree(TreeNode.Block);

 if not assigned(TreeNode.Left.Return) then begin
  TreeNode.Return:=TreeNode.Left.Return;
 end;
end;

procedure TOptimizerHighLevel.OptimizeREPEAT(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 if assigned(TreeNode.Left.Return) and (TreeNode.Left.Return.TypeDefinition=ttdBoolean) then begin
  OptimizeTree(TreeNode.Right);
  if LocalSwitches^.Optimization then begin
  end;
 end else begin
  Error.AbortCode(11);
  exit;
 end;
end;

procedure TOptimizerHighLevel.OptimizeWHILE(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 if assigned(TreeNode.Left.Return) and (TreeNode.Left.Return.TypeDefinition=ttdBoolean) then begin
  if LocalSwitches^.Optimization and (TreeNode.Left.TreeNodeType=ttntORDConst) and (TreeNode.Left.Value=0) and not (assigned(TreeNode.Right) and TreeNode.Right.CheckForNodes([ttntLABEL,ttntASM,ttntCCODE,ttntCEXPRESSION])) then begin
   // Dead code found - Kill it :-)
   Error.AddHintCode(139);
   TreeNode.TreeNodeType:=ttntEMPTY;
   FreeAndNil(TreeNode.Left);
   FreeAndNil(TreeNode.Right);
  end else begin
   OptimizeTree(TreeNode.Right);
  end;
 end else begin
  Error.AbortCode(11);
  exit;
 end;
end;

procedure TOptimizerHighLevel.OptimizeIF(var TreeNode:TTreeNode);
var Temp:TTreeNode;
begin
 OptimizeTree(TreeNode.Left);
 if assigned(TreeNode.Left.Return) and (TreeNode.Left.Return.TypeDefinition=ttdBoolean) then begin
//if (TreeNode.Left.TreeNodeType=ttntORDConst) or (assigned(TreeNode.Left.Return) and not (TreeNode.Left.Return.TypeDefinition=ttdBoolean)) then begin
  if LocalSwitches^.Optimization then begin
   if (TreeNode.Left.TreeNodeType=ttntORDConst) and (TreeNode.Left.Value=0) and not (assigned(TreeNode.Right) and TreeNode.Right.CheckForNodes([ttntLABEL,ttntASM,ttntCCODE,ttntCEXPRESSION])) then begin
    // Dead code found - Kill it :-)
    if assigned(TreeNode.ElseTree) then begin
     // If an ELSE tree node exist, then don't kill this ELSE tree node
     OptimizeTree(TreeNode.ElseTree);
     if assigned(TreeNode.ElseTree) and (TreeNode.ElseTree.TreeNodeType<>ttntEMPTY) then begin
      FreeAndNil(TreeNode.Left);
      FreeAndNil(TreeNode.Right);
      Temp:=TreeNode.ElseTree;
      TreeNode.ElseTree:=nil;
      FreeAndNil(TreeNode);
      TreeNode:=Temp;
{     TreeNode.Assign(Temp);
      Temp.Left:=nil;
      Temp.Right:=nil;
      Temp.Block:=nil;
      Temp.ElseTree:=nil;
      FreeAndNil(Temp);}
     end else begin
      TreeNode.TreeNodeType:=ttntEMPTY;
      FreeAndNil(TreeNode.Left);
      if assigned(TreeNode.Right) then begin
       FreeAndNil(TreeNode.Right);
      end;
      if assigned(TreeNode.ElseTree) then begin
       FreeAndNil(TreeNode.ElseTree);
      end;
     end;
    end else begin
     TreeNode.TreeNodeType:=ttntEMPTY;
     FreeAndNil(TreeNode.Left);
     FreeAndNil(TreeNode.Right);
    end;
    exit;
   end else if (TreeNode.Left.TreeNodeType=ttntORDConst) and (TreeNode.Left.Value=1) and ((assigned(TreeNode.ElseTree) and not TreeNode.ElseTree.CheckForNodes([ttntLABEL,ttntASM,ttntCCODE,ttntCEXPRESSION])) or not assigned(TreeNode.ElseTree)) then begin
    // Dead code found - Kill it :-)
    if assigned(TreeNode.Right) then begin
     // If an ELSE tree node exist, then don't kill this ELSE tree node
     OptimizeTree(TreeNode.Right);
     if assigned(TreeNode.Right) and (TreeNode.Right.TreeNodeType<>ttntEMPTY) then begin
      FreeAndNil(TreeNode.Left);
      if assigned(TreeNode.ElseTree) then begin
       FreeAndNil(TreeNode.ElseTree);
      end;
      Temp:=TreeNode.Right;
      TreeNode.Right:=nil;
      FreeAndNil(TreeNode);
      TreeNode:=Temp;
{     TreeNode.Assign(Temp);
      Temp.Left:=nil;
      Temp.Right:=nil;
      Temp.Block:=nil;
      Temp.ElseTree:=nil;
      FreeAndNil(Temp);}
     end else begin
      TreeNode.TreeNodeType:=ttntEMPTY;
      FreeAndNil(TreeNode.Left);
      if assigned(TreeNode.Right) then begin
       FreeAndNil(TreeNode.Right);
      end;
      if assigned(TreeNode.ElseTree) then begin
       FreeAndNil(TreeNode.ElseTree);
      end;
     end;
    end else begin
     TreeNode.TreeNodeType:=ttntEMPTY;
     FreeAndNil(TreeNode.Left);
     if assigned(TreeNode.ElseTree) then begin
      FreeAndNil(TreeNode.ElseTree);
     end;
    end;
    exit;
   end else begin
    OptimizeTree(TreeNode.Right);
    OptimizeTree(TreeNode.ElseTree);
    if ((assigned(TreeNode.Right) and (TreeNode.Right.TreeNodeType=ttntEMPTY)) or not assigned(TreeNode.Right)) and not
       (TreeNode.Left.CheckForNodes([ttntCALL,ttntCCODE,ttntCEXPRESSION]) and not (assigned(TreeNode.ElseTree) and (TreeNode.ElseTree.TreeNodeType<>ttntEMPTY))) then begin
     if assigned(TreeNode.Right) and (TreeNode.Right.TreeNodeType=ttntEMPTY) then FreeAndNil(TreeNode.Right);
     if assigned(TreeNode.ElseTree) and (TreeNode.ElseTree.TreeNodeType=ttntEMPTY) then FreeAndNil(TreeNode.ElseTree);
     if not (assigned(TreeNode.Right) or assigned(TreeNode.ElseTree)) then begin
      // Kill "if X=1 then;" without function calls
      TreeNode.TreeNodeType:=ttntEMPTY;
      FreeAndNil(TreeNode.Left);
     end else if assigned(TreeNode.ElseTree) and not assigned(TreeNode.Right) then begin
      // Convert "if X=1 then begin end else XYZ;" to "if not (X=1) then XYZ;" -> OptimizeTree -> "if X<>1 then XYZ;"
      Temp:=TreeNode.Left;
      TreeNode.Left:=TreeManager.GenerateLeftNode(ttntNOT,Temp);
      TreeNode.Right:=TreeNode.ElseTree;
      TreeNode.ElseTree:=nil;
      OptimizeTree(TreeNode.Left);
      OptimizeTree(TreeNode.Right);
     end;
    end else begin
     if (assigned(TreeNode.Right) and (TreeNode.Right.TreeNodeType=ttntEMPTY)) and
        (assigned(TreeNode.ElseTree) and (TreeNode.ElseTree.TreeNodeType<>ttntEMPTY)) then begin
      // Convert "if X=1 then begin end else XYZ;" to "if not (X=1) then XYZ;" -> OptimizeTree -> "if X<>1 then XYZ;"
      TreeNode.Left:=TreeManager.GenerateLeftNode(ttntNOT,TreeNode.Left);
      FreeAndNil(TreeNode.Right);
      TreeNode.Right:=TreeNode.ElseTree;
      TreeNode.ElseTree:=nil;
      OptimizeTree(TreeNode.Left);
      OptimizeTree(TreeNode.Right);
     end else begin
      // Okay okay, it's an normal IF case :-)
      OptimizeTree(TreeNode.Left);
      OptimizeTree(TreeNode.Right);
      OptimizeTree(TreeNode.ElseTree);
     end;
    end;
   end;
  end else begin
   OptimizeTree(TreeNode.Left);
   OptimizeTree(TreeNode.Right);
   OptimizeTree(TreeNode.ElseTree);
  end;
 end else begin
  Error.AbortCode(11);
  exit;
 end;
end;

procedure TOptimizerHighLevel.OptimizeCASE(var TreeNode:TTreeNode);
begin
 // TODO: Dead code detection + preevaluate
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
 OptimizeTree(TreeNode.ElseTree);
end;

procedure TOptimizerHighLevel.OptimizeCASEBlock(var TreeNode:TTreeNode);
begin
 if assigned(TreeNode.Left) and (TreeNode.Left.TreeNodeType=ttntCASEBlock) then begin
  OptimizeTree(TreeNode.Left);
 end;
 OptimizeTree(TreeNode.Right);
 OptimizeTree(TreeNode.Block);
end;

procedure TOptimizerHighLevel.OptimizeEmptyTrees(var TreeNode:TTreeNode);
begin
 if assigned(TreeNode) then begin
  if TreeNode.TreeNodeType=ttntEMPTY then begin
   FreeAndNil(TreeNode);
  end else begin
   OptimizeEmptyTrees(TreeNode.Left);
   OptimizeEmptyTrees(TreeNode.Right);
   OptimizeEmptyTrees(TreeNode.ElseTree);
   OptimizeEmptyTrees(TreeNode.ExceptTree);
   OptimizeEmptyTrees(TreeNode.FinallyTree);
   OptimizeEmptyTrees(TreeNode.Block);
  end;
 end;
end;

procedure TOptimizerHighLevel.OptimizeTRY(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Block);
 OptimizeTree(TreeNode.ExceptTree);
 OptimizeTree(TreeNode.FinallyTree);
end;

procedure TOptimizerHighLevel.OptimizeTRYONELSE(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
end;

procedure TOptimizerHighLevel.OptimizeRAISE(var TreeNode:TTreeNode);
begin
 OptimizeTree(TreeNode.Left);
 OptimizeTree(TreeNode.Right);
end;

procedure TOptimizerHighLevel.OptimizeTree(var TreeNode:TTreeNode);
begin
 if assigned(TreeNode) and not (TreeNode.DoNotOptimize or Error.DoAbort) then begin
  Initialize;
  CorrectCExpressionTypes(TreeNode.Left,TreeNode.Right);
  CorrectCExpressionTypes(TreeNode.Right,TreeNode.Left);
  case TreeNode.TreeNodeType of
   ttntEMPTY:begin
   end;
   ttntSubRange:begin
    OptimizeSubRange(TreeNode);
   end;
   ttntAssign:begin
    OptimizeAssign(TreeNode);
   end;
   ttntLess,ttntLessOrEqual,ttntGreater,ttntGreaterOrEqual,ttntEqual,ttntNotEqual:begin
    OptimizeCompare(TreeNode);
   end;
   ttntAdd:begin
    OptimizeAdd(TreeNode);
   end;
   ttntSub:begin
    OptimizeSub(TreeNode);
   end;
   ttntOr:begin
    OptimizeOr(TreeNode);
   end;
   ttntXor:begin
    OptimizeXor(TreeNode);
   end;
   ttntIn:begin
    OptimizeIn(TreeNode);
   end;
   ttntMul:begin
    OptimizeMul(TreeNode);
   end;
   ttntSlash:begin
    OptimizeSlash(TreeNode);
   end;
   ttntDiv:begin
    OptimizeDiv(TreeNode);
   end;
   ttntMod:begin
    OptimizeMod(TreeNode);
   end;
   ttntAnd:begin
    OptimizeAnd(TreeNode);
   end;
   ttntShl:begin
    OptimizeShl(TreeNode);
   end;
   ttntShr:begin
    OptimizeShr(TreeNode);
   end;
   ttntMinus:begin
    OptimizeMinus(TreeNode);
   end;
   ttntNot:begin
    OptimizeNot(TreeNode);
   end;
   ttntNil:begin
   end;
   ttntFloat:begin
   end;
   ttntBlock:begin
    OptimizeBlock(TreeNode);
   end;
   ttntStatement:begin
   end;
   ttntASM:begin
   end;
   ttntVAR:begin
    OptimizeVar(TreeNode);
   end;
   ttntTYPE:begin
    OptimizeType(TreeNode);
   end;
   ttntTYPEINFO:begin
   end;
   ttntTYPECONV:begin
    OptimizeTypeConv(TreeNode);
   end;
   ttntTYPECHECK:begin
    OptimizeTypeCheck(TreeNode);
   end;
   ttntFOR:begin
    OptimizeFOR(TreeNode);
   end;
   ttntWHILE:begin
    OptimizeWHILE(TreeNode);
   end;
   ttntREPEAT:begin
    OptimizeREPEAT(TreeNode);
   end;
   ttntIF:begin
    OptimizeIF(TreeNode);
   end;
   ttntWITH:begin
    OptimizeWITH(TreeNode);
   end;
   ttntCASE:begin
    OptimizeCASE(TreeNode);
   end;
   ttntCASEBLOCK:begin
    OptimizeCASEBlock(TreeNode);
   end;
   ttntCASEValue:begin
   end;
   ttntBREAK:begin
   end;        
   ttntCONTINUE:begin
   end;
   ttntEXIT:begin
   end;
   ttntLABEL:begin
   end;
   ttntGOTO:begin
   end;
   ttntTRY:begin
    OptimizeTRY(TreeNode);
   end;
   ttntTRYONELSE:begin
    OptimizeTRYONELSE(TreeNode);
   end;
   ttntRAISE:begin
    OptimizeRAISE(TreeNode);
   end;
   ttntPROCEDURE:begin
   end;
   ttntCALL:begin
    OptimizeCall(TreeNode);
   end;
   ttntParameter:begin
//  OptimizeParameter(TreeNode);
   end;
   ttntIndex:begin
    OptimizeIndex(TreeNode);
   end;
   ttntPointer:begin
    OptimizePointer(TreeNode);
   end;
   ttntAddress:begin
    OptimizeAddress(TreeNode);
   end;
   ttntField:begin
    OptimizeField(TreeNode);
   end;
   ttntORDConst:begin
    OptimizeOrdinalConst(TreeNode);
   end;
   ttntCHARConst:begin
    OptimizeCharConst(TreeNode);
   end;
   ttntSTRINGConst:begin
    OptimizeStringConst(TreeNode);
   end;
   ttntFloatConst:begin
    OptimizeFloatConst(TreeNode);
   end;
   ttntSETConst:begin
    OptimizeSetConst(TreeNode);
   end;
   ttntPCHARConst:begin
   end;
   ttntCCODE:begin
    OptimizeTree(TreeNode.Left);
   end;
   ttntCEXPRESSION:begin
    OptimizeTree(TreeNode.Left);
   end;
   ttntCBLOCK:begin
    OptimizeTree(TreeNode.Left);
   end;
   ttntPASCALBLOCK:begin
    OptimizeTree(TreeNode.Left);
   end;
   ttntCLOCATION:begin
   end;
   ttntTEMPOBJECT:begin
   end;
  end;
  if assigned(TreeNode) then begin
   OptimizeEmptyTrees(TreeNode.Left);
   OptimizeEmptyTrees(TreeNode.Right);
   OptimizeEmptyTrees(TreeNode.ElseTree);
   OptimizeEmptyTrees(TreeNode.ExceptTree);
   OptimizeEmptyTrees(TreeNode.FinallyTree);
   OptimizeEmptyTrees(TreeNode.Block);
  end;
 end;
end;

end.
