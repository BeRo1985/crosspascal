unit Error;
{$i Compiler.inc}

interface

uses Globals,StringList;

type TErrorPosition=record
      FileName:ansistring;
      Line:longint;
      Column:longint;
     end;

     TError=class
      private
       Options:POptions;
       procedure AddError(S:ansistring);
       procedure AddWarning(S:ansistring);
       procedure AddHint(S:ansistring);
      public
       LocalSwitches:PLocalSwitches;
       CurrentFileName:ansistring;
       CurrentLine:longint;
       CurrentColumn:longint;
       Errors:boolean;
       Warnings:boolean;
       Hints:boolean;
       DoAbort:boolean;
       PositionStack:array of TErrorPosition;
       ErrorList:TStringList;
       WarningList:TStringList;
       HintList:TStringList;
       constructor Create(TheOptions:POptions);
       destructor Destroy; override;
       procedure Append(From:TError);
       procedure SetFilePosition(FileName:ansistring;Line,Column:longint);
       procedure SetFilePositionColumn(Column:longint);
       procedure Push;
       procedure Pop;
       procedure ClearStack;
       function GetCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring=''):ansistring;
       procedure AddErrorCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
       procedure AddWarningCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
       procedure AddHintCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
       procedure AbortCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
       procedure InternalError(ErrorCode:int64);
       function Text:ansistring;
     end;

implementation

uses BeRoUtils,Symbols;

constructor TError.Create(TheOptions:POptions);
begin
 inherited Create;
 Options:=TheOptions;
 CurrentFileName:='';
 CurrentLine:=0;
 CurrentColumn:=0;
 Warnings:=false;
 Hints:=false;
 Errors:=false;
 DoAbort:=false;
 PositionStack:=nil;
 LocalSwitches:=nil;
 ErrorList:=TStringList.Create;
 WarningList:=TStringList.Create;
 HintList:=TStringList.Create;
end;

destructor TError.Destroy;
begin
 ErrorList.Destroy;
 WarningList.Destroy;
 HintList.Destroy;
 SetLength(PositionStack,0);
 inherited Destroy;
end;

procedure TError.Append(From:TError);
begin
 if assigned(From) then begin
  ErrorList.AddStrings(From.ErrorList);
  WarningList.AddStrings(From.WarningList);
  HintList.AddStrings(From.HintList);
  Errors:=Errors or From.Errors;
  Warnings:=Warnings or From.Warnings;
  Hints:=Hints or From.Hints;
  DoAbort:=DoAbort or From.DoAbort;
 end;
end;

procedure TError.SetFilePosition(FileName:ansistring;Line,Column:longint);
begin
 CurrentFileName:=FileName;
 CurrentLine:=Line;
 CurrentColumn:=Column;
end;

procedure TError.SetFilePositionColumn(Column:longint);
begin
 CurrentColumn:=Column;
end;

procedure TError.Push;
var index:longint;
begin
 index:=length(PositionStack);
 SetLength(PositionStack,index+1);
 PositionStack[index].FileName:=CurrentFileName;
 PositionStack[index].Line:=CurrentLine;
 PositionStack[index].Column:=CurrentColumn;
end;

procedure TError.Pop;
begin
 if length(PositionStack)>0 then begin
  SetLength(PositionStack,length(PositionStack)-1);
 end;
end;

procedure TError.ClearStack;
begin
 SetLength(PositionStack,0);
end;

procedure TError.AddError(S:ansistring);
var PositionIndex:longint;
    FileName,LineStr,ColumnStr:ansistring;
begin
 Errors:=true;
 if length(PositionStack)>0 then begin
  PositionIndex:=length(PositionStack)-1;
  FileName:=PositionStack[PositionIndex].FileName;
  STR(PositionStack[PositionIndex].Line,LineStr);
  STR(PositionStack[PositionIndex].Column,ColumnStr);
 end else begin
  FileName:=CurrentFileName;
  STR(CurrentLine,LineStr);
  STR(CurrentColumn,ColumnStr);
 end;
 S:='['+FileName+']('+LineStr+','+ColumnStr+'): '+S;
 if ErrorList.Find(S)<0 then begin
  ErrorList.Add(S);
 end;
end;

procedure TError.AddWarning(S:ansistring);
var PositionIndex:longint;
    FileName,LineStr,ColumnStr:ansistring;
begin
 if (not assigned(LocalSwitches)) or LocalSwitches^.Warnings then begin
  Warnings:=true;
  if length(PositionStack)>0 then begin
   PositionIndex:=length(PositionStack)-1;
   FileName:=PositionStack[PositionIndex].FileName;
   STR(PositionStack[PositionIndex].Line,LineStr);
   STR(PositionStack[PositionIndex].Column,ColumnStr);
  end else begin
   FileName:=CurrentFileName;
   STR(CurrentLine,LineStr);
   STR(CurrentColumn,ColumnStr);
  end;
  S:='['+FileName+']('+LineStr+','+ColumnStr+'): '+S;
  if WarningList.Find(S)<0 then begin
   WarningList.Add(S);
  end;
 end;
end;

procedure TError.AddHint(S:ansistring);
var PositionIndex:longint;
    FileName,LineStr,ColumnStr:ansistring;
begin
 if (not assigned(LocalSwitches)) or LocalSwitches^.Hints then begin
  Hints:=true;
  if length(PositionStack)>0 then begin
   PositionIndex:=length(PositionStack)-1;
   FileName:=PositionStack[PositionIndex].FileName;
   STR(PositionStack[PositionIndex].Line,LineStr);
   STR(PositionStack[PositionIndex].Column,ColumnStr);
  end else begin
   FileName:=CurrentFileName;
   STR(CurrentLine,LineStr);
   STR(CurrentColumn,ColumnStr);
  end;
  S:='['+FileName+']('+LineStr+','+ColumnStr+'): '+S;
  if HintList.Find(S)<0 then begin
   HintList.Add(S);
  end;
 end;
end;

function TError.GetCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring=''):ansistring;
begin
 case ErrorCode of
  0..99:begin
   case ErrorCode of
    0:result:='Ordinal type required';
    1:result:='File type not allowed here';
    2:result:='Undeclared identifier: "'+S1+'"';
    3:result:='Identifier redeclared: "'+S1+'"';
    4:result:='"'+S1+'" is not a type identifier';
    5:result:='PACKED not allowed here';
    6:result:='Constant or type identifier expected';
    7:result:='Incompatible types';
    8:result:='Incompatible types: "'+S1+'"';
    9:result:='Incompatible types: "'+S1+'" and "'+S2+'"';
    10:result:='Low bound exceeds high bound';
    11:result:='Type of expression must be BOOLEAN';
    12:result:='Type of expression must be INTEGER';
    13:result:='Statement expected, but expression of type "'+S1+'" found';
    14:result:='Operator not applicable to this operand type';
    15:result:='Array type required';
    17:result:='Record, object or class type required';
    18:result:='Object type required';
    19:result:='Object or class type required';
    20:result:='Class type required';
    21:result:='Function needs result type';
    22:result:='Invalid function result type';
    23:result:='Procedure cannot have a result type';
    24:result:='Text after final "END." - ignored by compiler';
    25:result:='Constant expression expected';
    26:result:='Constant expression violates subrange bounds';
    27:result:='Duplicate tag value';
    28:result:='Sets may have at most 256 elements';
    29:result:='"'+S1+'" expected but "'+S2+'" found';
    30:result:='Duplicate case label';
    31:result:='Label expected';
    32:result:='For loop control variable must be simple local variable';
    33:result:='For loop control variable must have ordinal type';
    34:result:='Types of actual and formal var parameters must be identical';
    35:result:='Too many actual parameters';
    36:result:='Not enough actual parameters';
    37:result:='Variable required';
    38:result:='Declaration of "'+S1+'" differs from previous declaration';
    39:result:='Illegal character in input file: "'+S1+'" ($'+S2+')';
    40:result:='File not found: "'+S1+'"';
    41:result:='Could not create output file "'+S1+'"';
    42:result:='Seek error on "'+S1+'"';
    44:result:='Write error on "'+S1+'"';
    45:result:='Close error on "'+S1+'"';
    46:result:='Bad file format: "'+S1+'"';
    47:result:='Out of memory';
    48:result:='Circular unit reference between "'+S1+'" and "'+S2+'"';
    49:result:='Bad unit format: "'+S1+'"';
    50:result:='Label declaration not allowed in interface part';
    51:result:='Statements not allowed in interface part';
    52:result:='Unit "'+S1+'" was compiled with a different version of "'+S2+'"';
    53:result:='Unterminated string';
    54:result:='Syntax error in real number';
    55:result:='Illegal type in Write/Writeln statement';
    56:result:='Illegal type in Read/Readln statement';
    57:result:='Strings may have at most 255 elements';
    58:result:='Unexpected end of file in comment started on line '+S1;
    59:result:='Invalid compiler directive: "'+S1+'"';
    60:result:='Bad global symbol definition: "'+S1+'" in object file "'+S2+'"';
    61:result:='Class or object types only allowed in type section';
    62:result:='Local class or object types not allowed';
    63:result:='Virtual constructors are not allowed';
    64:result:='Could not compile used unit "'+S1+'"';
    65:result:='Left side cannot be assigned to';
    66:result:='Unsatisfied forward or external declaration: "'+S1+'"';
    67:result:='Missing operator or semicolon';
    68:result:='Missing parameter type';
    69:result:='Illegal reference to symbol "'+S1+'"in object file "'+S2+'"';
    70:result:='Line too long (more than 255 characters)';
    71:result:='Unknown directive: "'+S1+'"';
    72:result:='This type cannot be initialized';
    73:result:='Number of elements differs from declaration';
    74:result:='Label already defined: "'+S1+'"';
    75:result:='Label declared and referenced, but not set: "'+S1+'"';
    78:result:='Variable "'+S1+'" might not have been initialized';
    79:result:='Value assigned to "'+S1+'" never used';
    80:result:='Return value of function "'+S1+'" might be undefined';
    81:result:='Procedure FAIL only allowed in constructor';
    82:result:='Procedure NEW needs constructor';
    83:result:='Procedure DISPOSE needs destructor';
    84:result:='Assignment to FOR-Loop variable "'+S1+'"';
    85:result:='FOR-Loop variable "'+S1+'" may be undefined after loop';
    86:result:='TYPEOF can only be applied to object types with a VMT';
    87:result:='Order of fields in record constant differs from declaration';
    88:result:='Internal error: '+S1;
    89:result:='Unit name mismatch: "'+S1+'" in "'+S2+'"';
    90:result:='Type "'+S1+'" is not yet completely defined';
    91:result:='This Demo Version has been patched';
    92:result:='Integer constant or variable name expected';
    93:result:='Invalid typecast';
    94:result:='User break - compilation aborted';
    95:result:='Assignment to typed constant "'+S1+'"';
    96:result:='Segment/Offset pairs not supported in 32-bit Pascal';
    97:result:='Program or unit "'+S1+'" recursively uses itself';
    98:result:='Label "'+S1+'" is not declared in current procedure';
    99:result:='Local procedure/function "'+S1+'" assigned to procedure variable';
    else result:='';
   end;
  end;
  100..199:begin
   case ErrorCode of
    100:result:='Missing ENDIF directive';
    101:result:='Method identifier expected';
    102:result:='FOR-Loop variable "'+S1+'" cannot be passed as var parameter';
    103:result:='Typed constant "'+S1+'" passed as var parameter';
    104:result:='BREAK or CONTINUE outside of loop';
    105:result:='Division by zero';
    106:result:='Overflow in conversion or arithmetic operation';
    107:result:='Data type too large: exceeds 2 GB';
    108:result:='Integer constant too large';
    110:result:='Inline assembler syntax error';
    111:result:='Inline assembler stack overflow';
    112:result:='Operand size mismatch';
    113:result:='Memory reference expected';
    114:result:='Constant expected';
    115:result:='Type expected';
    116:result:='Cannot add or subtract relocatable symbols';
    117:result:='Invalid register combination';
    118:result:='Numeric overflow';
    119:result:='String constant too long';
    120:result:='Error in numeric constant';
    121:result:='Invalid combination of opcode and operands';
    122:result:='486/487 instructions not enabled';
    123:result:='Division by zero';
    124:result:='Structure field identifier expected';
    125:result:='LOOP/JCXZ distance out of range';
    126:result:='Procedure or function name expected';
    127:result:='PROCEDURE or FUNCTION expected';
    128:result:='Instance variable "'+S1+'" inaccessible here';
    129:result:='EXCEPT or FINALLY expected';
    130:result:='Cannot BREAK, CONTINUE or EXIT out of a FINALLY clause';
    131:result:='"GOTO '+S1+'" leads into or out of TRY statement';
    132:result:='"'+S1+'" clause expected, but "'+S2+'" found';
    133:result:='Cannot assign to a read-only property';
    134:result:='Cannot read a write-only property';
    135:result:='Class already has a default property';
    136:result:='Default property must be an array property';
    137:result:='TYPEINFO standard function expects a type identifier';
    138:result:='Type "'+S1+'" has no type info';
    139:result:='FOR or WHILE loop executes zero times - deleted';
    140:result:='No definition for abstract method "'+S1+'" allowed';
    141:result:='Method "'+S1+'" not found in base class';
    142:result:='Invalid message parameter list';
    143:result:='Illegal message method index';
    144:result:='Duplicate dynamic method index';
    145:result:='Bad file format "'+S1+'"';
    146:result:='Inaccessible value';
    147:result:='Destination cannot be assigned to';
    148:result:='Expression has no value';
    149:result:='Destination is inaccessible';
    150:result:='Re-raising an exception only allowed in exception handler';
    151:result:='Default values must be of ordinal, pointer or small set type';
    152:result:='Property "'+S1+'" does not exist in base class';
    153:result:='Dynamic method or message handler not allowed here';
    154:result:='Class does not have a default property';
    155:result:='Bad argument type in variable type array constructor';
    156:result:='Could not load RLINK32.DLL';
    157:result:='Wrong or corrupted version of RLINK32.DLL';
    158:result:='";" not allowed before "ELSE"';
    159:result:='Type "'+S1+'" needs finalization - not allowed in variant record';
    160:result:='Type "'+S1+'" needs finalization - not allowed in file type';
    161:result:='Expression too complicated';
    162:result:='Element 0 inaccessible - use "Length" or "SetLength"';
    163:result:='System unit out of date or corrupted: missing "'+S1+'"';
    164:result:='Type not allowed in OLE Automation call';
    165:result:='RLINK32 error';
    166:result:='RLINK32 error';
    167:result:='Too many conditional symbols';
    168:result:='Method "'+S1+'" hides virtual method of base type "'+S2+'"';
    169:result:='Variable "'+S1+'" is declared but never used in "'+S2+'"';
    170:result:='Compile terminated by user';
    171:result:='Unnamed arguments must precede named arguments in OLE Automation call';
    172:result:='Abstract methods must be virtual or dynamic';
    173:result:='Case label outside of range of case expression';
    174:result:='Field or method identifier expected';
    175:result:='Constructing instance of "'+S1+'" containing abstract methods';
    176:result:='Field definition not allowed after methods or properties';
    177:result:='Cannot override a static method';
    178:result:='Variable "'+S1+'" inaccessible here due to optimization';
    179:result:='Necessary library helper function was eliminated by linker';
    180:result:='Missing or invalid conditional symbol in "$'+S1+'" directive';
    181:result:='"'+S1+'" not previously declared as a PROPERTY';
    182:result:='Field definition not allowed in OLE automation section';
    183:result:='Illegal type in OLE automation section: "'+S1+'"';
    184:result:='String constant truncated to fit STRING['+S1+']';
    185:result:='Constructors and destructors not allowed in OLE automation section';
    186:result:='Dynamic methods and message handlers not allowed in OLE automation section';
    187:result:='Only register calling convention allowed in OLE automation section';
    188:result:='Dispid "'+S1+'" already used by "'+S2+'"';
    189:result:='Redeclaration of property not allowed in OLE automation section';
    190:result:='"'+S1+'" clause not allowed in OLE automation section';
    191:result:='Dispid clause only allowed in OLE automation section';
    192:result:='Type "'+S1+'" must be a class to have OLE automation';
    193:result:='Type "'+S1+'" must be a class to have a PUBLISHED section';
    194:result:='Redeclaration of "'+S1+'" hides a member in the base class';
    195:result:='Overriding automated virtual method "'+S1+'" cannot specify a dispid';
    196:result:='Published Real48 property "'+S1+'" must be Single, Real, Double or Extended';
    197:result:='Size of published set "'+S1+'" is >32 bits';
    198:result:='Published property "'+S1+'" cannot be of type "'+S2+'"';
    199:result:='Thread local variables cannot be local to a function';
    else result:='';
   end;
  end;
  200..299:begin
   case ErrorCode of
    200:result:='Thread local variables cannot be ABSOLUTE';
    201:result:='EXPORTS allowed only at global scope';
    202:result:='Constants cannot be used as open array arguments';
    203:result:='Slice standard function only allowed as open array argument';
    204:result:='Cannot initialize thread local variables';
    205:result:='Cannot initialize local variables';
    206:result:='Cannot initialize multiple variables';
    207:result:='Constant object cannot be passed as var parameter';
    208:result:='HIGH cannot be applied to a long string';
    209:result:='Unit "'+S1+'" implicitly imported into package "'+S2+'"';
    210:result:='Packages "'+S1+'" and "'+S2+'" both contain unit "'+S3+'"';
    211:result:='Packages "'+S1+'" already constains unit "'+S2+'"';
    212:result:='File not found: "'+S1+'.cuf"';
    213:result:='Need imported data reference ($G) to access "'+S1+'" from unit "'+S2+'"';
    214:result:='Required package "'+S1+'" not found';
    215:result:='$WEAKPACKAGEUNIT "'+S1+'" contains global data';
    216:result:='Improper GUID syntax';
    217:result:='Interface type required';
    218:result:='Property overrides not allowed in interface type';
    219:result:='"'+S1+'" clause not allowed in interface type';
    220:result:='Interface "'+S1+'" already implemented by "'+S2+'"';
    221:result:='Field declarations not allowed in interface type';
    222:result:='"'+S1+'" directive not allowed in interface type';
    223:result:='Declaration of "'+S1+'" differs from declaration in interface "'+S2+'"';
    224:result:='Package unit "'+S1+'" cannot appear in contains or uses clauses';
    225:result:='Bad packaged unit format: '+S1+'.'+S2+'';
    226:result:='Package "'+S1+'" is recursively required';
    228:result:='Published field "'+S1+'" not a class nor interface type';
    229:result:='Private symbol "'+S1+'" declared but never used';
    230:result:='Could not compile package "'+S1+'"';
    231:result:='Never-build package "'+S1+'" requires always-build package "'+S2+'"';
    232:result:='$WEAKPACKAGEUNIT "'+S1+'" cannot have initialization or finalization code';
    233:result:='$WEAKPACKAGEUNIT & $DENYPACKAGEUNIT both specified';
    234:result:='$DENYPACKAGEUNIT "'+S1+'" cannot be put into a package';
    235:result:='$DESIGNONLY and $RUNONLY only allowed in package unit';
    236:result:='Never-build package "'+S1+'" must be recompiled';
    237:result:='Compilation terminated; too many errors';
    238:result:='Imagebase is too high - program exceeds 2 GB limit';
    239:result:='A dispinterface type cannot have an ancestor interface';
    240:result:='A dispinterface type requires an interface identification';
    241:result:='Methods of dispinterface types cannot specify directives';
    242:result:='"'+S1+'" directive not allowed in dispinterface type';
    243:result:='Interface "'+S1+'" has no interface identification';
    244:result:='Property "'+S1+'" inaccessible here';
    245:result:='Unsupported language feature: "'+S1+'"';
    246:result:='Getter or setter for property "'+S1+'" cannot be found';
    247:result:='Package "'+S1+'" does not use or export "'+S2+'.'+S3+'"';
    248:result:='Constructors and destructors must have register calling convention';
    249:result:='Parameter "'+S1+'" not allowed here due to default value';
    250:result:='Default value required for "'+S1+'"';
    251:result:='Default parameter "'+S1+'" must be by-value or const';
    252:result:='Constant 0 converted to NIL';
    253:result:='$EXTERNALSYM and $NODEFINE not allowed for "'+S1+'"; only global symbols';
    254:result:='$HPPEMIT "'+S1+'" ignored';
    255:result:='Bad file format: "'+S1+'"';
    256:result:='C++ obj files must be generated (-jp)';
    257:result:='"'+S1+'" is not the name of a unit';
    258:result:='Expression needs no Initialize/Finalize';
    259:result:='Pointer expression needs no Initialize/Finalize - need ^ operator?';
    260:result:='Recursive include file "'+S1+'"';
    261:result:='Need to specify at least one dimension for SetLength of dynamic array';
    262:result:='Cannot take the address when compiling to byte code';
    263:result:='Cannot use old style object types when compiling to byte code';
    264:result:='Cannot use absolute variables when compiling to byte code';
    265:result:='There is no overloaded version of "'+S1+'" that can be called with these arguments';
    266:result:='Ambiguous overloaded call to "'+S1+'"';
    267:result:='Method "'+S1+'" with identical parameters exists already';
    268:result:='Ancestor type "'+S1+'" does not have default constructor';
    269:result:='Overloaded procedure "'+S1+'" must be marked with the "overload" directive';
    270:result:='Class methods not allowed as property getters or setters';
    271:result:='New not supported for dynamic arrays - use SetLength';
    272:result:='Dispose not supported (nor necessary) for dynamic arrays';
    273:result:='Duplicate implements clause for interface "'+S1+'"';
    274:result:='Implements clause only allowed within class types';
    275:result:='Implements clause only allowed for properties of class or interface type';
    276:result:='Implements clause not allowed together with index clause';
    277:result:='Implements clause only allowed for readable property';
    278:result:='Implements getter must be register calling convention';
    279:result:='Implements getter cannot be dynamic or message method';
    280:result:='Cannot have method resolutions for interface "'+S1+'"';
    281:result:='Interface "'+S1+'" not mentioned in interface list';
    282:result:='Exported package threadvar "'+S1+'.'+S2+'" cannot be used outside of this package';
    283:result:='Only one of a set of overloaded methods can be published';
    284:result:='Previous declaration of "'+S1+'" was not marked with the "overload" directive';
    285:result:='Parameters of this type cannot have default values';
    286:result:='Overriding virtual method "'+S1+'.'+S2+'" has a lower visibility than base class';
    287:result:='Published property getters and setters must have register calling convention';
    288:result:='Property getters and setters cannot be overloaded';
    289:result:='Comparing signed and unsigned types - widened both operands';
    290:result:='Combining signed and unsigned types - widened both operands';
    291:result:='Duplicate "'+S1+'.'+S2+'" with identical parameters will be inaccessible from C++';
    292:result:='Comparison always evaluates to False';
    293:result:='Comparison always evaluates to True';
    294:result:='Cannot use reserved unit name "'+S1+'"';
    295:result:='No overloaded version of "'+S1+'" with this parameter list exists';
    296:result:='Property attribute "label" cannot be used in dispinterface';
    297:result:='Property attribute "label" cannot be an empty string';
    else result:='';
   end;
  end;
  500..599:begin
   case ErrorCode of
    500:result:='Possible loss of data';
    501:result:='Invalid qualifier';
    502:result:='Field declarations not allowed in interface type';
    503:result:='Record, object, class or interface has no fields or methods';
    504:result:='Error in expression';
    505:result:='Method "'+S1+'" in one of parented classes not found';
    506:result:='This method doesn''t have any parent';
    507:result:='General syntax error';
    508:result:='Field identifier expected';
    509:result:='"'+'" not allowed here';
    510:result:='Class or interface type required';
    511:result:='Invalid parameter';
    512:result:='Procedure, function, constructor or destructor expected';
    513:result:='Calling convention of "'+S1+'" differs from previous declaration';
    514:result:='Parameters of "'+S1+'" differs from previous declaration';
    515:result:='Duplicate ordinal value';
    516:result:='Empty records not allowed';
    517:result:='Global variable expected';
    518:result:='Global variable, function or procedure expected';
    519:result:='Pointer type required';
    520:result:='Type of expression must be FLOAT';
    521:result:='A dynamic array cann''t used as an constant';
    522:result:='A open array cann''t used as an constant';
    523:result:='Unknown field "'+S1+'"';
    524:result:='Invalid typed constant';
    525:result:='Unknown identifier referenced';
    526:result:='Integer constant expected';
    527:result:='Variable or memory address expected';
    528:result:='Undefined type in pointer definition: "'+S1+'"';
    529:result:='Unresolved class forward definition: "'+S1+'"';
    530:result:='Error at loading unit "'+S1+'"';
    531:result:='Unit "'+S1+'" redeclared';
    532:result:='Unresolved symbol fixups';
    else result:='';
   end;
  end;
  5000..5099:begin
   case ErrorCode of
    5000:result:='Extra data in record';
    5001:result:='No header record';
    5002:result:='Record data not present';
    5003:result:='Insufficient memory';
    5004:result:='Invalid data address';
    5005:result:='Invalid segment number';
    5006:result:='Invalid fixup record';
    5007:result:='Invalid segment definition record';
    5008:result:='Data emitted to absolute segment';
    5009:result:='Duplicate public definition';
    5010:result:='Unexpected end of file';
    5011:result:='Duplicate module header';
    5012:result:='Unknown object module record type "'+S1+'"';
    5013:result:='4 gb non-absolute segments not supported';
    5014:result:='Start address defined in more than one module';
    5015:result:='Illegal group definition';
    5017:result:='Overlapping data regions';
    5018:result:='COMENT record format invalid';
    5019:result:='Illegal imports';
    5020:result:='Invalid or corrupt COFF object file';
    5021:result:='Unsupported CPU type';
    5022:result:='Optional header discarded';
    5023:result:='Invalid or corrupt COFF object file, unable to read string table size';
    5024:result:='Invalid or corrupt COFF object file, bad string table size';
    5025:result:='Invalid or corrupt COFF object file, unable to read string table';
    5026:result:='Invalid or corrupt COFF object file, last string unterminated';
    5027:result:='Invalid or corrupt COFF object file, unable to read symbols';
    5028:result:='Invalid or corrupt COFF object file, bad symbol name location';
    5029:result:='Invalid or corrupt COFF object file, unknown symbol class';
    5030:result:='Invalid or corrupt COFF object file, unable to read symbol aux records';
    5031:result:='Invalid or corrupt COFF object file, unable to read section headers';
    5032:result:='Invalid or corrupt COFF object file, invalid number';
    5033:result:='Invalid or corrupt COFF object file, unable to read section name';
    5034:result:='Duplicate public symbol "'+S1+'"';
    5035:result:='Invalid or corrupt COFF object file, bad section alignment';
    5036:result:='Invalid COMDAT section reference';
    5037:result:='Invalid COMDAT section';
    5038:result:='Invalid COMDAT symbol';
    5039:result:='COMDATs not yet supported';
    5040:result:='Invalid or corrupt COFF object file, unable to read sections';
    5041:result:='Invalid or corrupt COFF object file, unable to read relocations';
    5042:result:='Invalid or corrupt COFF object file, undefined symbol';
    5043:result:='Invalid or corrupt COFF object file, cannot relocate against a debug info symbol';
    5044:result:='Undefined symbol "'+S1+'"';
    5045:result:='Invalid or corrupt COFF object file, unknown relocation type';
    else result:='';
   end;
  end;
  10000..10099:begin
   case ErrorCode of
    10000:result:='Compilation error';
    10001:result:='Out of registers';
    10002:result:='Error at loading address of variable';
    10003:result:='Invalid record';
    10004:result:='Wrong count of actual parameters';
    else result:='';
   end;
  end;
  else result:='';
 end;
end;

procedure TError.AddErrorCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
var S:ansistring;
begin
 S:=GetCode(ErrorCode,S1,S2,S3);
 if length(S)=0 then begin
  S:='Unknown error';
 end;
//S:=INTTOSTR(ErrorCode)+'. '+S;
 AddError(S);
end;

procedure TError.AddWarningCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
var S:ansistring;
begin
 S:=GetCode(ErrorCode,S1,S2,S3);
 if length(S)=0 then begin
  S:='Unknown warning';
 end;
//S:=INTTOSTR(ErrorCode)+'. '+S;
 AddWarning(S);
end;

procedure TError.AddHintCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
var S:ansistring;
begin
 S:=GetCode(ErrorCode,S1,S2,S3);
 if length(S)=0 then begin
  S:='Unknown hint';
 end;
//S:=INTTOSTR(ErrorCode)+'. '+S;
 AddHint(S);
end;

procedure TError.AbortCode(ErrorCode:int64;S1:ansistring='';S2:ansistring='';S3:ansistring='');
begin
 if not DoAbort then begin
  AddErrorCode(ErrorCode,S1,S2,S3);
  DoAbort:=true;
 end;
end;

procedure TError.InternalError(ErrorCode:int64);
begin
 AbortCode(88,INTTOSTR(ErrorCode));
end;

function TError.Text:ansistring;
var index:longint;
begin
 result:='';
 for index:=0 to ErrorList.Count-1 do begin
  result:=result+'ERROR: '+ErrorList[index]+#13#10;
 end;
 for index:=0 to WarningList.Count-1 do begin
  result:=result+'WARNING: '+WarningList[index]+#13#10;
 end;
 for index:=0 to HintList.Count-1 do begin
  result:=result+'HINT: '+HintList[index]+#13#10;
 end;
end;

end.
