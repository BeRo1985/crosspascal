unit Code; 
{$i Compiler.inc}

interface

uses BeRoUtils,BeRoStream,Globals,Error,Symbols,Tree,PointerList,CRC32;

type TCode=class
      private
       Options:POptions;
       procedure GenerateCodeBlock(ContextSymbol:PSymbol;TreeNode:TTreeNode);
       procedure GenerateCode(ContextSymbol:PSymbol;TreeNode:TTreeNode);
       procedure EmitConstants(ModuleSymbol:PSymbol);
       procedure EmitSymbols(ModuleSymbol:PSymbol);
      public
       CurrentLevel:longint;
       DebugLine:longint;

       Error:TError;
       SymbolManager:TSymbolManager;
       TreeManager:TTreeManager;
       LocalSwitches:PLocalSwitches;

       AlignValue:longword;

       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheTreeManager:TTreeManager;TheOptions:POptions;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;

       procedure BeginRootNestedProc; virtual;
       procedure EndRootNestedProc; virtual;

       procedure GenerateProc(ProcSymbol:PSymbol;ProcCodeTree:TTreeNode); virtual;
       procedure GenerateProgram(ProgramSymbol:PSymbol;ProgramCodeTree:TTreeNode); virtual;
       procedure GenerateLibrary(LibrarySymbol:PSymbol;LibraryCodeTree:TTreeNode); virtual;
       procedure GeneratePackage(PackageSymbol:PSymbol); virtual;
       procedure GenerateUnit(UnitSymbol:PSymbol;InitializationCodeTree,FinalizationCodeTree:TTreeNode); virtual;

       procedure SaveToStreams(CodeStream,HeaderStream:TBeRoStream); virtual;
     end;

implementation

constructor TCode.Create(TheError:TError;TheSymbolManager:TSymbolManager;TheTreeManager:TTreeManager;TheOptions:POptions;TheLocalSwitches:PLocalSwitches);
begin
 inherited Create;
 Options:=TheOptions;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 TreeManager:=TheTreeManager;
 LocalSwitches:=TheLocalSwitches;
 AlignValue:=4;
end;

destructor TCode.Destroy;
begin
 inherited Destroy;
end;

procedure TCode.EmitConstants(ModuleSymbol:PSymbol);
{var Size,LastSize,Counter,SubCounter,StringIDNr,Len:longint;
    Constant:PConstant;
    EncodedString:ansistring;
    C:ansichar;
    WC:widechar;}
begin
{ASMStream.WriteLine('.ALIGN 16');
 Constant:=SymbolManager.ConstantTable.First;
 LastSize:=0;
 StringIDNr:=0;
 while assigned(Constant) do begin
  if (Constant^.OwnerModule=ModuleSymbol) and not Constant^.Dumped then begin
   if assigned(Constant^.Previous) and (Constant^.Previous^.ID<>Constant^.ID) then begin
    ASMStream.WriteLine('.ALIGN '+INTTOSTR(AlignValue));
   end;
   case Constant^.ConstantType of
    tctVMT:begin
//   Constant^.StringValue
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
     Size:=0;
    end;
    tctPOINTER:begin
     Size:=4;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     if assigned(Constant^.PointerTo) then begin
      ASMStream.WriteLine('DD OFFSET '+Constant^.PointerTo^.OverloadedName);
     end else begin
      ASMStream.WriteLine('DD 0');
     end;
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctSTRING,tctWIDESTRING,tctPCHAR,tctPWIDECHAR:begin
     Size:=4;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     case Constant^.ConstantType of
      tctSTRING,tctPCHAR:Len:=length(Constant^.StringValue);
      tctWIDESTRING,tctPWIDECHAR:Len:=length(Constant^.WideStringValue);
      else Len:=0;
     end;
     if Len=0 then begin
      Constant^.IDNr:=-1;
     end else begin
      Constant^.IDNr:=StringIDNr;
     end;
     if Constant^.IDNr<0 then begin
      ASMStream.WriteLine('DD 0');
     end else begin
      inc(StringIDNr);
      ASMStream.WriteLine('DD OFFSET _STRING$'+LongwordToHex(Constant^.IDNr));
     end;
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctDATASTRING:begin
     Size:=Constant^.Size+1;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     EncodedString:='';
     for Counter:=1 to length(Constant^.StringValue) do begin
      C:=Constant^.StringValue[Counter];
      case C of
       #0:EncodedString:=EncodedString+'\0';
       #7:EncodedString:=EncodedString+'\b';
       #8:EncodedString:=EncodedString+'\u';
       #9:EncodedString:=EncodedString+'\t';
       #10:EncodedString:=EncodedString+'\n';
       #13:EncodedString:=EncodedString+'\r';
       #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
       '''','"','\':EncodedString:=EncodedString+'\'+C;
       else EncodedString:=EncodedString+C;
      end;
     end;
     for Counter:=length(Constant^.StringValue)+1 to Constant^.Size do begin
      EncodedString:=EncodedString+'\0';
     end;
     EncodedString:=EncodedString+'\0';
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctSHORTSTRING:begin
     Size:=Constant^.Size;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     EncodedString:='';
     ASMStream.WriteLine('DB '+INTTOSTR(byte(Constant^.ShortStringValue[0])));
     for Counter:=1 to length(Constant^.ShortStringValue) do begin
      C:=Constant^.ShortStringValue[Counter];
      case C of
       #0:EncodedString:=EncodedString+'\0';
       #7:EncodedString:=EncodedString+'\b';
       #8:EncodedString:=EncodedString+'\u';
       #9:EncodedString:=EncodedString+'\t';
       #10:EncodedString:=EncodedString+'\n';
       #13:EncodedString:=EncodedString+'\r';
       #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
       '''','"','\':EncodedString:=EncodedString+'\'+C;
       else EncodedString:=EncodedString+C;
      end;
     end;
     for Counter:=length(Constant^.StringValue)+1 to Constant^.Size-1 do begin
      EncodedString:=EncodedString+'\0';
     end;
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctOrdinal:begin
     Size:=Constant^.Size;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     case Constant^.Size of
      1:ASMStream.WriteLine('DB '+INTTOSTR(Constant^.IntValue));
      2:ASMStream.WriteLine('DW '+INTTOSTR(Constant^.IntValue));
      4:ASMStream.WriteLine('DD '+INTTOSTR(Constant^.IntValue));
      8:ASMStream.WriteLine('DQ '+INTTOSTR(Constant^.IntValue));
      else ASMStream.WriteLine('DD '+INTTOSTR(Constant^.IntValue));
     end;
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctChar:begin
     Size:=1;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     EncodedString:='';
     C:=Constant^.CharValue;
     case C of
      #0:EncodedString:=EncodedString+'\0';
      #7:EncodedString:=EncodedString+'\b';
      #8:EncodedString:=EncodedString+'\u';
      #9:EncodedString:=EncodedString+'\t';
      #10:EncodedString:=EncodedString+'\n';
      #13:EncodedString:=EncodedString+'\r';
      #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
      '''','"','\':EncodedString:=EncodedString+'\'+C;
      else EncodedString:=EncodedString+C;
     end;
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctWideChar:begin
     Size:=2;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     EncodedString:='';
     WC:=Constant^.WideCharValue;
     for SubCounter:=0 to 1 do begin
      C:=pansichar(pointer(@WC))[SubCounter];
      case C of
       #0:EncodedString:=EncodedString+'\0';
       #7:EncodedString:=EncodedString+'\b';
       #8:EncodedString:=EncodedString+'\u';
       #9:EncodedString:=EncodedString+'\t';
       #10:EncodedString:=EncodedString+'\n';
       #13:EncodedString:=EncodedString+'\r';
       #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
       '''','"','\':EncodedString:=EncodedString+'\'+C;
       else EncodedString:=EncodedString+C;
      end;
     end;
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctFloat:begin
     Size:=Constant^.Size;
     if not (assigned(Constant^.Previous) and (Constant^.Previous^.ID=Constant^.ID)) then begin
      ASMStream.WriteLine(Constant^.ID+':');
     end;
     case Constant^.Size of
      4:ASMStream.WriteLine('DSF '+FLOATTOSTR(Constant^.FloatValue));
      8:ASMStream.WriteLine('DDF '+FLOATTOSTR(Constant^.FloatValue));
      10:ASMStream.WriteLine('DEF '+FLOATTOSTR(Constant^.FloatValue));
      else ASMStream.WriteLine('DSF '+FLOATTOSTR(Constant^.FloatValue));
     end;
     if assigned(Constant^.Next) and (Constant^.ID<>Constant^.Next^.ID) then begin
      ASMStream.WriteLine('');
     end;
    end;
    tctAlign:begin
     if assigned(Constant^.Previous) then begin
      Size:=LastSize;
     end else begin
      Size:=0;
     end;
    end;
    else Size:=0;
   end;
   LastSize:=Size;
   if not Constant^.IsPacked then begin
    if (AlignValue<>0) and ((Size and (AlignValue-1))<>0) then begin
     ASMStream.WriteLine('RESB '+INTTOSTR(AlignValue-(Size and (AlignValue-1))));
     LastSize:=AlignValue;
    end;
   end;
   Constant^.Dumped:=true;
  end;
  Constant:=Constant^.Next;
 end;
 ASMStream.WriteLine('');

 Constant:=SymbolManager.ConstantTable.First;
 while assigned(Constant) do begin
  if (Constant^.OwnerModule=ModuleSymbol) and (Constant^.IDNr>=0) and not Constant^.StringDumped then begin
   case Constant^.ConstantType of
    tctSTRING:begin
     ASMStream.WriteLine('.ALIGN '+INTTOSTR(AlignValue));
     ASMStream.WriteLine('DD 0,'+INTTOSTR(length(Constant^.StringValue)));
     ASMStream.WriteLine('_STRING$'+LongwordToHex(Constant^.IDNr)+':');
     EncodedString:='';
     for Counter:=1 to length(Constant^.StringValue) do begin
      C:=Constant^.StringValue[Counter];
      case C of
       #0:EncodedString:=EncodedString+'\0';
       #7:EncodedString:=EncodedString+'\b';
       #8:EncodedString:=EncodedString+'\u';
       #9:EncodedString:=EncodedString+'\t';
       #10:EncodedString:=EncodedString+'\n';
       #13:EncodedString:=EncodedString+'\r';
       #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
       '''','"','\':EncodedString:=EncodedString+'\'+C;
       else EncodedString:=EncodedString+C;
      end;
     end;
     EncodedString:=EncodedString+'\0';
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     ASMStream.WriteLine('');
    end;
    tctWIDESTRING:begin
     ASMStream.WriteLine('.ALIGN '+INTTOSTR(AlignValue));
     ASMStream.WriteLine('DD 0,'+INTTOSTR(length(Constant^.WideStringValue)));
     ASMStream.WriteLine('_STRING$'+LongwordToHex(Constant^.IDNr)+':');
     EncodedString:='';
     for Counter:=1 to length(Constant^.WideStringValue) do begin
      WC:=Constant^.WideStringValue[Counter];
      for SubCounter:=0 to 1 do begin
       C:=pansichar(pointer(@WC))[SubCounter];
       case C of
        #0:EncodedString:=EncodedString+'\0';
        #7:EncodedString:=EncodedString+'\b';
        #8:EncodedString:=EncodedString+'\u';
        #9:EncodedString:=EncodedString+'\t';
        #10:EncodedString:=EncodedString+'\n';
        #13:EncodedString:=EncodedString+'\r';
        #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
        '''','"','\':EncodedString:=EncodedString+'\'+C;
        else EncodedString:=EncodedString+C;
       end;
      end;
     end;
     EncodedString:=EncodedString+'\0\0';
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     ASMStream.WriteLine('');
    end;
    tctPCHAR:begin
     ASMStream.WriteLine('.ALIGN '+INTTOSTR(AlignValue));
     ASMStream.WriteLine('_STRING$'+LongwordToHex(Constant^.IDNr)+':');
     EncodedString:='';
     for Counter:=1 to length(Constant^.StringValue) do begin
      C:=Constant^.StringValue[Counter];
      case C of
       #0:EncodedString:=EncodedString+'\0';
       #7:EncodedString:=EncodedString+'\b';
       #8:EncodedString:=EncodedString+'\u';
       #9:EncodedString:=EncodedString+'\t';
       #10:EncodedString:=EncodedString+'\n';
       #13:EncodedString:=EncodedString+'\r';
       #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
       '''','"','\':EncodedString:=EncodedString+'\'+C;
       else EncodedString:=EncodedString+C;
      end;
     end;
     EncodedString:=EncodedString+'\0';
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     ASMStream.WriteLine('');
    end;
    tctPWIDECHAR:begin
     ASMStream.WriteLine('.ALIGN '+INTTOSTR(AlignValue));
     ASMStream.WriteLine('_STRING$'+LongwordToHex(Constant^.IDNr)+':');
     EncodedString:='';
     for Counter:=1 to length(Constant^.WideStringValue) do begin
      WC:=Constant^.WideStringValue[Counter];
      for SubCounter:=0 to 1 do begin
       C:=pansichar(pointer(@WC))[SubCounter];
       case C of
        #0:EncodedString:=EncodedString+'\0';
        #7:EncodedString:=EncodedString+'\b';
        #8:EncodedString:=EncodedString+'\u';
        #9:EncodedString:=EncodedString+'\t';
        #10:EncodedString:=EncodedString+'\n';
        #13:EncodedString:=EncodedString+'\r';
        #1..#6,#11..#12,#14..#31:EncodedString:=EncodedString+'\x'+ByteToHex(byte(C));
        '''','"','\':EncodedString:=EncodedString+'\'+C;
        else EncodedString:=EncodedString+C;
       end;
      end;
     end;
     EncodedString:=EncodedString+'\0\0';
     ASMStream.WriteLine('DSTR "'+EncodedString+'"');
     ASMStream.WriteLine('');
    end;
   end;
   Constant^.StringDumped:=true;
  end;
  Constant:=Constant^.Next;
 end;
 ASMStream.WriteLine('');}
end;

procedure TCode.EmitSymbols(ModuleSymbol:PSymbol);
begin
end;

procedure TCode.GenerateCodeBlock(ContextSymbol:PSymbol;TreeNode:TTreeNode);
var Node:TTreeNode;
begin
 Node:=TreeNode.Left;
 while assigned(Node) do begin
  if assigned(Node.Right) then begin
   GenerateCode(ContextSymbol,Node.Right);
  end;
  Node:=Node.Left;
 end;
end;

procedure TCode.GenerateCode(ContextSymbol:PSymbol;TreeNode:TTreeNode);
begin
 if assigned(TreeNode) then begin
  case TreeNode.TreeNodeType of
   ttntBlock:begin
    GenerateCodeBlock(ContextSymbol,TreeNode);
   end;
  end;
 end;
end;

procedure TCode.BeginRootNestedProc;
begin
end;

procedure TCode.EndRootNestedProc;
begin
end;

procedure TCode.GenerateProc(ProcSymbol:PSymbol;ProcCodeTree:TTreeNode);
begin
 GenerateCode(ProcSymbol,ProcCodeTree);
 EmitConstants(ProcSymbol);
// EmitSymbols(ProcSymbol); // needed???
end;

procedure TCode.GenerateProgram(ProgramSymbol:PSymbol;ProgramCodeTree:TTreeNode);
begin
 GenerateCode(ProgramSymbol,ProgramCodeTree);
 EmitConstants(ProgramSymbol);
 EmitSymbols(ProgramSymbol);
 // Emit here code from all previous GenerateCode calls
end;

procedure TCode.GenerateLibrary(LibrarySymbol:PSymbol;LibraryCodeTree:TTreeNode);
begin
 GenerateCode(LibrarySymbol,LibraryCodeTree);
 EmitConstants(LibrarySymbol);
 EmitSymbols(LibrarySymbol);
 // Emit here code from all previous GenerateCode calls
end;

procedure TCode.GeneratePackage(PackageSymbol:PSymbol);
begin
 EmitConstants(PackageSymbol);
 EmitSymbols(PackageSymbol);
 // Emit here code from all previous GenerateCode calls
end;

procedure TCode.GenerateUnit(UnitSymbol:PSymbol;InitializationCodeTree,FinalizationCodeTree:TTreeNode);
var UnitName:ansistring;
begin
 UnitName:=UnitSymbol^.name;
 if assigned(InitializationCodeTree) then begin
  GenerateCode(UnitSymbol,InitializationCodeTree);
 end;
 if assigned(FinalizationCodeTree) then begin
  GenerateCode(UnitSymbol,FinalizationCodeTree);
 end;
 EmitConstants(UnitSymbol);
 EmitSymbols(UnitSymbol);
 // Emit here code from all previous GenerateCode calls
end;

procedure TCode.SaveToStreams(CodeStream,HeaderStream:TBeRoStream);
begin
end;

end.


