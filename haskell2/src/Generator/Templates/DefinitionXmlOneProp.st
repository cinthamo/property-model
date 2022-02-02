        <Prop>
          <Id>$p.id$</Id>
          <Name>$p.name$</Name>
          <Type>$p.aType$</Type>
$if(p.customType)$
          <Default>$p.customType$</Default>
$endif$
$if(p.aDefault)$
          <Default>$p.aDefault$</Default>
$endif$
        </Prop>

