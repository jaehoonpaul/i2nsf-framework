<?xml version="1.0" encoding="ISO-8859-1"?>
<!--

  A reasonable confspec to yang translator. Takes one conspec file and
  does as best it can on it to produce valid yang output.

  Features and limitations:

  - preserves comments
  - xs:list and structuredType only works in single file
  - by default namespace is taken from targetNamespace attribute
  - by default prefix is taken from prefix attribute (or from targetNamespace)
  - by default module is the same as the filename
    (All three can be passed as parameters, see cs2yang shell-script)
  
  Note: unhandled features are copied into resulting yang as comments
  (look for 'FIXME cs2yang:' in the resulting file)

  TODO:
    - recursive expansion of list/structured types
    - structured types with keys
    - fix keyref predicates
    - fix default-ref
  -->
<xsl:stylesheet version="1.0"
		xmlns:cs="http://tail-f.com/ns/confspec/1.0"
		xmlns:csa="http://tail-f.com/ns/confspec/annotations/1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:str="http://exslt.org/strings"
		xmlns:dyn="http://exslt.org/dynamic"
		xmlns:confd="http://tail-f.com/ns/confd/1.0"
		xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>

  <!-- Global parameters -->
  <xsl:param name="gModule"/>
  <xsl:param name="gNamespace"/>
  <xsl:param name="gPrefix"/>
  <xsl:param name="gFilename"/>

  <!-- Global variables -->
  <xsl:variable name="newline">
<xsl:text>
</xsl:text>
  </xsl:variable>
  <xsl:variable name="QuoteChar"><xsl:text>&quot;</xsl:text></xsl:variable>
  <xsl:variable name="singleQuoteChar"><xsl:text>'</xsl:text></xsl:variable>

  <xsl:variable name="ns-xs"    select="'http://www.w3.org/2001/XMLSchema'"/>
  <xsl:variable name="ns-confd" select="'http://tail-f.com/ns/confd/1.0'"/>
  <xsl:variable name="ns-smi"   select="'http://tail-f.com/ns/mibs/smi/1.0'"/>
  <xsl:variable name="ns-snmpv2tc" select="'http://tail-f.com/ns/mibs/SNMPv2-TC/1.0'"/>

  <!--
     - Helper functions
    -->

  <!-- Indent to the current level -->
  <xsl:template name="indent">
    <xsl:param name="extra" select="'0'"/>
    <xsl:variable name="level">
      <xsl:call-template name="depth">
	<xsl:with-param name="node" select="."/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="str:padding('2' * $level + $extra)"/>
  </xsl:template>

  <!-- Return the current depth -->
  <xsl:template name="depth">
    <xsl:param name="node"/>
    <xsl:param name="depth" select="'-2'"/>
    <xsl:choose>
      <xsl:when test="not($node)">
	<!-- <xsl:message><xsl:value-of select="concat('TOP: ',name($node),' ',$node/@name,' ',generate-id($node))"/></xsl:message> -->
	<xsl:value-of select="$depth"/>
      </xsl:when>
      <xsl:otherwise>
	<!-- <xsl:message><xsl:value-of select="concat(name($node),' ',$node/@name,' ',generate-id($node))"/></xsl:message> -->
	<xsl:call-template name="depth">
	  <xsl:with-param name="node" select="$node/.."/>
	  <xsl:with-param name="depth" select="$depth + '1'"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- insert quotes around a string if needed -->
  <xsl:template name="quote">
    <xsl:param name="str"/>

    <!-- FIXME inefficient... -->
    <!-- FIXME doesn't properly escape \ and double quotes -->
    <xsl:variable name="quotep">
      <xsl:choose>
	<xsl:when test="$str = ''"/>
	<xsl:when test="not($str)"/>
	<xsl:when test="contains($str, ' ')"/>
	<xsl:when test="contains($str, ';')"/>
	<xsl:when test="contains($str, '{')"/>
	<xsl:when test="contains($str, '}')"/>
	<xsl:when test="contains($str, '/')"/>
	<xsl:when test="contains($str, '\')"/>
	<xsl:when test="contains($str, $QuoteChar)"/>
	<xsl:when test="contains($str, $singleQuoteChar)"/>
	<xsl:when test="starts-with($str, '[')"/>
	<xsl:otherwise>
	  <xsl:value-of select="'no'"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$quotep != 'no'">
	<xsl:choose>
	  <xsl:when test="contains($str, $QuoteChar)">
	    <xsl:choose>
	      <xsl:when test="contains($str, $singleQuoteChar)">
		<!-- both single and double quotes in string -->
		<!-- solve by replacing double quotes with single -->
		<!-- FIXME should instead escape double -->
		<xsl:value-of select="concat($QuoteChar, translate($str,$QuoteChar,$singleQuoteChar), $QuoteChar)"/>
	      </xsl:when>
	      <xsl:otherwise>
		<!-- double quotes in string, quote with single -->
		<xsl:value-of select="concat($singleQuoteChar, $str, $singleQuoteChar)"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:when>
	  <xsl:when test="contains($str, '\')">
	    <xsl:choose>
	      <xsl:when test="contains($str, $singleQuoteChar)">
		<!-- both single quotes and backslash in string -->
		<!-- solve by replacing single quotes with double -->
		<!-- FIXME should instead escape backslash -->
		<xsl:value-of select="concat($singleQuoteChar, translate($str,$singleQuoteChar,$QuoteChar), $singleQuoteChar)"/>
	      </xsl:when>
	      <xsl:otherwise>
		<!-- backslash in string, quote with single quote -->
		<xsl:value-of select="concat($singleQuoteChar, $str, $singleQuoteChar)"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:when>
	  <xsl:otherwise>
	    <!-- no quote or backslash characters in string -->
	    <xsl:value-of select="concat($QuoteChar, $str, $QuoteChar)"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$str"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- emit a yang statement -->
  <xsl:template name="emit-statement">
    <xsl:param name="keyw"/>
    <xsl:param name="arg"/>
    <xsl:param name="extra"  select="'0'"/>     <!-- indentation -->
    <xsl:param name="argp"   select="true()"/>	<!-- use argument? -->
    <xsl:param name="quotep" select="true()"/>  <!-- quote arg? -->
    <xsl:param name="blockp" select="false()"/> <!-- begin block? -->
    <xsl:param name="nlp"    select="true()"/>	<!-- break line? -->

    <xsl:call-template name="indent">
      <xsl:with-param name="extra" select="$extra"/>
    </xsl:call-template>
    <xsl:value-of select="$keyw"/>

    <xsl:if test="string($argp) = 'true'">
      <!-- possibly quote the arg string -->
      <xsl:variable name="qarg">
	<xsl:choose>
	  <xsl:when test="$quotep">
	    <xsl:call-template name="quote">
	      <xsl:with-param name="str" select="$arg"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$arg"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:value-of select="concat(' ', $qarg)"/>
    </xsl:if>

    <!-- terminate with ; or start a block with { followed by WS or \n -->
    <xsl:choose>
      <xsl:when test="$blockp">
	<xsl:text> {</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$nlp">
	<xsl:value-of select="$newline"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text> </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    
  <!-- emit a yang single statement with an unquoted argument -->
  <xsl:template name="emit-statement-unquoted">
    <xsl:param name="keyw"/>
    <xsl:param name="arg"/>
    <xsl:param name="extra" select="'0'"/>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw"   select="$keyw"/>
      <xsl:with-param name="extra"  select="$extra"/>
      <xsl:with-param name="arg"    select="$arg"/>
      <xsl:with-param name="quotep" select="false()"/>
    </xsl:call-template>
  </xsl:template>

  <!-- As emit-statement, except arg is a nodeset and the resulting
       arg is the @name of each member of that nodeset -->
  <xsl:template name="emit-statement-nodenames">
    <xsl:param name="keyw"/>
    <xsl:param name="arg"/>
    <xsl:param name="extra" select="'0'"/>
    <xsl:variable name="argstr">
      <xsl:for-each select="$arg">
	<xsl:value-of select="concat(' ', @name)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="$keyw"/>
      <xsl:with-param name="arg" select="normalize-space($argstr)"/>
      <xsl:with-param name="extra" select="$extra"/>
    </xsl:call-template>
  </xsl:template>

  <!-- As emit-statement-nodenames, except that the resulting arg is
       the relative (datamodel) path to each member of the arg nodeset -->
  <xsl:template name="emit-statement-nodenames-relpath">
    <xsl:param name="keyw"/>
    <xsl:param name="arg"/>
    <xsl:param name="parent"/>
    <xsl:param name="extra" select="'0'"/>
    <xsl:variable name="argstr">
      <xsl:for-each select="$arg">
	<xsl:variable name="name">
	  <xsl:call-template name="relpath">
	    <xsl:with-param name="parent" select="$parent"/>
	    <xsl:with-param name="pos" select="."/>
	  </xsl:call-template>
	</xsl:variable>
	<xsl:value-of select="concat(' ', $name)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="$keyw"/>
      <xsl:with-param name="arg" select="normalize-space($argstr)"/>
      <xsl:with-param name="extra" select="$extra"/>
    </xsl:call-template>
  </xsl:template>

  <!-- emit a yang statement and start a new block -->
  <xsl:template name="emit-statement-block">
    <xsl:param name="keyw"/>
    <xsl:param name="arg"/>
    <xsl:param name="extra" select="'0'"/>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="$keyw"/>
      <xsl:with-param name="extra" select="$extra"/>
      <xsl:with-param name="arg" select="$arg"/>
      <xsl:with-param name="blockp" select="true()"/>
    </xsl:call-template>
  </xsl:template>

  <!-- emit a yang closing bracket -->
  <xsl:template name="emit-close-block">
    <xsl:param name="extra" select="'0'"/>
    <xsl:call-template name="indent">
      <xsl:with-param name="extra" select="$extra"/>
    </xsl:call-template>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <!-- If there is a desc() tag below us, emit a yang description stmt -->
  <xsl:template name="emit-description">
    <xsl:if test="cs:desc/text()">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'tailf:info'"/>
	<xsl:with-param name="arg" select="cs:desc/text()"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <!-- emit translation of common attributes -->
  <xsl:template name="translate-attribute">
    <xsl:param name="name"/>
    <xsl:param name="value"/>

    <xsl:variable name="keyw">
      <xsl:choose>
	<xsl:when test="($name = 'config') or ($name = 'default')">
	  <xsl:value-of select="$name"/>
	</xsl:when>
	<xsl:when test="$name = 'orderedBy'">
	  <xsl:value-of select="'ordered-by'"/>
	</xsl:when>
	<xsl:when test="$name = 'callOnce'">
	  <xsl:value-of select="'tailf:call-once'"/>
	</xsl:when>
	<xsl:when test="$name = 'writable'">
	  <xsl:value-of select="'tailf:writable'"/>
	</xsl:when>
	<xsl:when test="$name = 'cliShowNo'">
	  <xsl:value-of select="'tailf:cli-show-no'"/>
	</xsl:when>
	<xsl:when test="$name = 'constant'">
	  <xsl:value-of select="'tailf:constant-leaf'"/>
	</xsl:when>
	<xsl:when test="$name = 'defaultRef'">
	  <xsl:value-of select="'tailf:default-ref'"/>
	</xsl:when>
	<xsl:when test="$name = 'displayGroups'">
	  <xsl:value-of select="'tailf:display-groups'"/>
	</xsl:when>
	<xsl:when test="$name = 'hidden'">
	  <xsl:value-of select="'tailf:hidden'"/>
	</xsl:when>
	<xsl:when test="$name = 'idValue'">
	  <xsl:value-of select="'tailf:id-value'"/>
	</xsl:when>
	<xsl:when test="$name = 'sortOrder'">
	  <xsl:value-of select="'tailf:sort-order'"/>
	</xsl:when>
	<xsl:when test="$name = 'snmpOID'">
	  <xsl:value-of select="'tailf:snmp-oid'"/>
	</xsl:when>
	<xsl:when test="$name = 'snmpId'">
	  <xsl:value-of select="'tailf:snmp-name'"/>
	</xsl:when>
	<xsl:when test="$name = 'snmpMIBModule'">
	  <xsl:value-of select="'tailf:snmp-mib-module-name'"/>
	</xsl:when>
	<xsl:when test="$name = 'snmpRowStatusColumn'">
	  <xsl:value-of select="'tailf:snmp-row-status-column'"/>
	</xsl:when>
	<xsl:when test="$name = 'snmpLaxTypeCheck'">
	  <xsl:value-of select="'tailf:snmp-lax-type-check'"/>
	</xsl:when>
	<xsl:when test="$name = 'generatedName'">
	  <xsl:value-of select="'tailf:java-class-name'"/>
	</xsl:when>
	<xsl:when test="$name = 'cliName'">
	  <xsl:value-of select="'tailf:alt-name'"/>
	</xsl:when>
	<xsl:when test="$name = 'columnName'">
	  <xsl:value-of select="'tailf:display-column-name'"/>
	</xsl:when>
	<xsl:when test="$name = 'printName'">
	  <xsl:value-of select="'tailf:display-status-name'"/>
	</xsl:when>
	<xsl:when test="$name = 'cliCShowConfig'">
	  <xsl:value-of select="'tailf:cli-show-config'"/>
	</xsl:when>
	<xsl:when test="$name = 'sortPriority'">
	  <xsl:value-of select="'tailf:sort-priority'"/>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="value1">
      <xsl:choose>
        <xsl:when test="($keyw = 'tailf:sort-order') and ($value = 'snmp_implied')">
          <xsl:text>snmp-implied</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$value"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <xsl:choose>
      <xsl:when test="$name = 'name'"/>
      <xsl:when test="$name = 'exists'"/>
      <xsl:when test="$name = 'path_filters'"/>
      <xsl:when test="$keyw != ''">
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="$keyw"/>
	  <xsl:with-param name="arg"  select="$value1"/>
	  <xsl:with-param name="argp">
	    <xsl:choose>
	      <xsl:when
		 test="($keyw = 'tailf:cli-show-no') and ($value = 'true')">
		<xsl:text>false</xsl:text>
	      </xsl:when>
	      <xsl:when
		 test="($keyw = 'tailf:cli-show-config') and ($value = 'true')">
		<xsl:text>false</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text>true</xsl:text>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>/* FIXME cs2yang: unhandled attribute: </xsl:text>
	<xsl:value-of select="concat($name,' = ',$value1,' */',$newline)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- namespace of the module we are translating -->
  <xsl:template name="module-namespace">
    <xsl:param name="prefix" select="'example'"/>
    <xsl:choose>
      <xsl:when test="$gNamespace != ''">
	<xsl:value-of select="$gNamespace"/>
      </xsl:when>
      <xsl:when test="/cs:confspec/@targetNamespace">
	<xsl:value-of select="/cs:confspec/@targetNamespace"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="concat('http://www.example.com/ns/',$prefix,'/1.0')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="namespace-to-prefix">
    <xsl:param name="ns"/>
    <xsl:variable name="last" select="str:tokenize($ns,'/')[last()]"/>
    <xsl:variable name="last-as-number">
      <xsl:value-of select="number(substring($last,1,1))"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string($last-as-number) = 'NaN'">
	<xsl:value-of select="$last"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="str:tokenize($ns,'/')[last()-1]"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- emit module, namespace and prefix declaration -->
  <xsl:template name="emit-module-block">
    <!-- Start by figuring out module, namespace and prefix -->
    <xsl:variable name="prefix">
      <xsl:choose>
	<xsl:when test="$gPrefix != ''">
	  <xsl:value-of select="$gPrefix"/>
	</xsl:when>
	<xsl:when test="@prefix">
	  <xsl:value-of select="@prefix"/>
	</xsl:when>
	<xsl:when test="$gNamespace != ''">
	  <xsl:value-of select="str:tokenize($gNamespace, '/')[last()-1]"/>
	</xsl:when>
	<xsl:when test="@targetNamespace != ''">
	  <xsl:value-of select="str:tokenize(@targetNamespace, '/')[last()-1]"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$gFilename"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="ns">
      <xsl:call-template name="module-namespace">
	<xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="module">
      <xsl:choose>
	<xsl:when test="$gModule != ''">
	  <xsl:value-of select="$gModule"/>
	</xsl:when>
	<xsl:when test="@yangModuleName">
	  <xsl:value-of select="@yangModuleName"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$gFilename"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'module'"/>
      <xsl:with-param name="arg" select="$module"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'namespace'"/>
      <xsl:with-param name="arg" select="$ns"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'prefix'"/>
      <xsl:with-param name="arg" select="$prefix"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:id'"/>
      <xsl:with-param name="arg" select="/cs:confspec/@id"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="emit-submodule-block">
    <xsl:variable name="prefix">
      <xsl:choose>
	<xsl:when test="$gPrefix != ''">
	  <xsl:value-of select="$gPrefix"/>
	</xsl:when>
	<xsl:when test="@prefix">
	  <xsl:value-of select="@prefix"/>
	</xsl:when>
	<xsl:when test="$gNamespace != ''">
	  <xsl:value-of select="str:tokenize($gNamespace, '/')[last()-1]"/>
	</xsl:when>
	<xsl:when test="@targetNamespace != ''">
	  <xsl:value-of select="str:tokenize(@targetNamespace, '/')[last()-1]"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$gFilename"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="module">
      <xsl:choose>
	<xsl:when test="$gModule != ''">
	  <xsl:value-of select="$gModule"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$gFilename"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'submodule'"/>
      <xsl:with-param name="arg" select="$module"/>
    </xsl:call-template>
    <xsl:text>  /* cs2yang:
   * - Assuming this is a submodule because a unique mountNamespace
   *   attribute could not be found.
   * - Target namespace: </xsl:text>
    <xsl:value-of select="concat(@targetNamespace, $newline)"/>
    <xsl:text>   * - Guessing that the module it belongs to is called &quot;</xsl:text>
    <xsl:value-of select="concat($prefix,$QuoteChar,$newline)"/>
    <xsl:text>   * - Manually add &quot;include </xsl:text>
    <xsl:value-of select="$module"/>
    <xsl:text>;&quot; to module &quot;</xsl:text>
    <xsl:value-of select="concat($prefix,$QuoteChar,$newline, '   */', $newline)"/>
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'belongs-to'"/>
      <xsl:with-param name="arg" select="$prefix"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'prefix'"/>
      <xsl:with-param name="arg" select="$prefix"/>
      <xsl:with-param name="extra" select="'4'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-close-block">
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="emit-import-stmt">
    <xsl:param name="name"/>
    <xsl:param name="prefix"/>
    <xsl:param name="extra" select="'0'"/>
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'import'"/>
      <xsl:with-param name="arg"  select="$name"/>
      <xsl:with-param name="extra" select="'2' + $extra"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'prefix'"/>
      <xsl:with-param name="arg"  select="$prefix"/>
      <xsl:with-param name="extra" select="'4' + $extra"/>
    </xsl:call-template>
    <xsl:call-template name="emit-close-block">
      <xsl:with-param name="extra" select="'2' + $extra"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="relpath">
    <xsl:param name="parent"/>
    <xsl:param name="pos"/>
    <xsl:param name="path"/>
    <xsl:choose>
      <xsl:when test="generate-id($pos) = generate-id($parent)">
	<xsl:value-of select="$path"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="relpath">
	  <xsl:with-param name="parent" select="$parent"/>
	  <xsl:with-param name="pos" select="$pos/.."/>
	  <xsl:with-param name="path">
	    <xsl:choose>
	      <xsl:when test="$path">
		<xsl:value-of select="concat($pos/@name, '/', $path)"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@name"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <!-- Note: don't call this template with an attribute node as
       context node, because in that case the namespace axis is
       empty.  If the attribute is the context node you can change
       it before calling, like this:

  	   <xsl:for-each select="..">
             <xsl:call-template name="translate-type">
             ...
           <xsl:for-each/>

       I'm sure there is a better way, but I don't know what it is:-)
  -->
  <xsl:template name="translate-namespace-prefix">
    <xsl:param name="type-str"/>

    <xsl:variable name="ns-tag" select="substring-before($type-str, ':')"/>
    <!-- Here comes the "trick" of matching the prefix with the
	 declared namespaces prefixes -->
    <xsl:variable name="ns" select="string(namespace::*[name() = $ns-tag])"/>

    <!-- Sigh. For "snippets" of conspec there usually isn't a proper
         xmlns, so be kind and try to make it work... -->
    <xsl:choose>
      <xsl:when test="($ns = '') and ($ns-tag = 'xs')">
	<xsl:value-of select="$ns-xs"/>
      </xsl:when>
      <xsl:when test="($ns = '') and ($ns-tag = 'xsd')">
	<xsl:value-of select="$ns-xs"/>
      </xsl:when>
      <xsl:when test="($ns = '') and ($ns-tag = 'confd')">
	<xsl:value-of select="$ns-confd"/>
      </xsl:when>
      <xsl:when test="($ns = '') and ($ns-tag = 'smi')">
	<xsl:value-of select="$ns-smi"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$ns"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  
  <!-- Datatype mapping -->
  <xsl:template name="translate-type">
    <xsl:param name="type-str"/>

    <xsl:variable name="type" select="substring-after($type-str, ':')"/>
    <xsl:variable name="ns">
      <xsl:call-template name="translate-namespace-prefix">
	<xsl:with-param name="type-str" select="$type-str"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:choose>

      <xsl:when test="$ns = $ns-xs">
	<xsl:choose>
	  <!-- Translate XML Schema types into YANG types whenever possible -->

	  <!-- builtin types -->
	  <xsl:when test="$type = 'byte'">
	    <xsl:text>int8</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'short'">
	    <xsl:text>int16</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'int'">
	    <xsl:text>int32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'integer'">
	    <xsl:text>int64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'long'">
	    <xsl:text>int64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'unsignedByte'">
	    <xsl:text>uint8</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'unsignedShort'">
	    <xsl:text>uint16</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'unsignedInt'">
	    <xsl:text>uint32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'unsignedLong'">
	    <xsl:text>uint64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'string'">
	    <xsl:text>string</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'boolean'">
	    <xsl:text>boolean</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'dateTime'">
	    <xsl:text>yang:date-and-time</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'negativeInteger'">
	    <xsl:text>int64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'nonNegativeInteger'">
	    <xsl:text>uint64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'nonPositiveInteger'">
	    <xsl:text>int64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'positiveInteger'">
	    <xsl:text>uint64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'anyURI'">
	    <xsl:text>inet:uri</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'base64Binary'">
	    <xsl:text>binary</xsl:text>
	  </xsl:when>
	  <xsl:when test="starts-with($type, 'g')">
	    <xsl:text>empty /* FIXME cs2yang: type </xsl:text>
	    <xsl:value-of select="$type"/>
	    <xsl:text> not supported */</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <!-- remaining xml schema types live in tailf-xsd-types -->
	    <xsl:value-of select="concat('xs:', $type)"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>

      <xsl:when test="$ns = $ns-confd">
	<xsl:choose>
	  <!-- ConfD internal types -->
	  <xsl:when test="$type = 'inetAddress'">
	    <xsl:text>inet:host</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'inetAddressIP'">
	    <xsl:text>inet:ip-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'inetAddressIPv4'">
	    <xsl:text>inet:ipv4-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'inetAddressIPv6'">
	    <xsl:text>inet:ipv6-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'inetAddressDNS'">
	    <xsl:text>inet:domain-name</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'inetPortNumber'">
	    <xsl:text>inet:port-number</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'ipv4Prefix'">
	    <xsl:text>inet:ipv4-prefix</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'ipv6Prefix'">
	    <xsl:text>inet:ipv6-prefix</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'ipPrefix'">
	    <xsl:text>inet:ip-prefix</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'size'">
	    <xsl:text>tailf:size</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'MD5DigestString'">
	    <xsl:text>tailf:md5-digest-string</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'DES3CBCEncryptedString'">
	    <xsl:text>tailf:des3-cbc-encrypted-string</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'AESCFB128EncryptedString'">
	    <xsl:text>tailf:aes-cfb-128-encrypted-string</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Counter32'">
	    <xsl:text>yang:counter32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Counter64'">
	    <xsl:text>yang:counter64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Gauge32'">
	    <xsl:text>yang:gauge32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'objectRef'">
	    <xsl:text>instance-identifier</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'octetList'">
	    <xsl:text>tailf:octet-list</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'oid'">
	    <xsl:text>yang:object-identifier</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'hexList'">
	    <xsl:text>tailf:hex-list</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$type"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>

      <xsl:when test="$ns = $ns-smi">
	<xsl:choose>
	  <!-- SMIv2 types -->
	  <xsl:when test="$type = 'IpAddress'">
	    <xsl:text>inet:ipv4-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'NetworkAddress'">
	    <xsl:text>inet:ipv4-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Ipv6Address'">
	    <xsl:text>inet:ipv6-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'ObjectIdentifier'">
	    <xsl:text>yang:object-identifier</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'OctetString'">
	    <xsl:text>tailf:hex-list</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'DisplayString'">
	    <xsl:text>string</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Unsigned32'">
	    <xsl:text>uint32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Counter32'">
	    <xsl:text>yang:counter32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Counter'">
	    <xsl:text>yang:counter32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Gauge32'">
	    <xsl:text>yang:gauge32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Gauge'">
	    <xsl:text>yang:gauge32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Counter64'">
	    <xsl:text>yang:counter64</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'TimeTicks'">
	    <xsl:text>yang:timeticks</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Integer32'">
	    <xsl:text>int32</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Opaque'">
	    <xsl:text>tailf:hex-list</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'DateAndTime'">
	    <xsl:text>yang:date-and-time</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'Integer32'">
	    <xsl:text>int32</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$type"/>
	    <xsl:text> /* FIXME cs2yang: untranslated smi type */ </xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>

      <xsl:when test="$ns = $ns-snmpv2tc">
	<xsl:choose>
	  <xsl:when test="$type = 'TimeStamp'">
	    <xsl:text>yang:timestamp</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'PhysAddress'">
	    <xsl:text>yang:phys-address</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type = 'MacAddress'">
	    <xsl:text>yang:mac-address</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
            <!-- The rest live in SNMPv2-TC.yang -->
	    <xsl:value-of select="$type-str"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>

      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test="not($type-str)"> <!-- untyped optional leafs -->
	    <xsl:text>empty</xsl:text>
	  </xsl:when>
	  <xsl:when test="$type-str = ''"> <!-- untyped optional leafs -->
	    <xsl:text>empty</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$type-str"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- use 'tailf:value-length' instead of 'length' for some types -->
  <xsl:template name="length-keyword">
    <xsl:param name="type-str"/>

    <xsl:variable name="type" select="substring-after($type-str, ':')"/>
    <xsl:variable name="ns">
      <xsl:call-template name="translate-namespace-prefix">
	<xsl:with-param name="type-str" select="$type-str"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="(($ns = $ns-xs) and
                          ($type = 'hexBinary')) or
                      (($ns = $ns-confd) and
                         (($type = 'oid') or
                          ($type = 'octetList') or
                          ($type = 'hexList'))) or
                      (($ns = $ns-smi) and
                         (($type = 'ObjectIdentifier') or
                          ($type = 'OctetString') or
                          ($type = 'Opaque'))) or
                      (($ns = $ns-snmpv2tc) and
                          ($type = 'PhysAddress'))">
        <xsl:text>tailf:value-length</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>length</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- any implicit restrictions that follow from the type -->
  <xsl:template name="emit-type-implicit-restriction">
    <xsl:param name="type-str"/>
    <xsl:param name="extra" select="'2'"/>

    <xsl:variable name="type" select="substring-after($type-str, ':')"/>
    <xsl:variable name="ns">
      <xsl:call-template name="translate-namespace-prefix">
	<xsl:with-param name="type-str" select="$type-str"/>
      </xsl:call-template>
    </xsl:variable>

    <xsl:variable name="range">
      <xsl:choose>
	<xsl:when test="$ns = $ns-xs">
	  <xsl:choose>
	    <xsl:when test="$type = 'negativeInteger'">
	      <xsl:text>min .. -1</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type = 'nonNegativeInteger'">
	      <xsl:text>0 .. max</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type = 'nonPositiveInteger'">
	      <xsl:text>min .. 0</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type = 'positiveInteger'">
	      <xsl:text>1 .. max</xsl:text>
	    </xsl:when>
	  </xsl:choose>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:if test="$range != ''">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'range'"/>
	<xsl:with-param name="arg"  select="$range"/>
	<xsl:with-param name="extra"  select="$extra"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:variable name="length">
      <xsl:choose>
	<xsl:when test="$ns = $ns-smi">
	  <xsl:choose>
	    <xsl:when test="($type = 'DisplayString')
			    and not(xs:maxLength) and not(xs:minLength)">
	      <xsl:text>0 .. 255</xsl:text>
	    </xsl:when>
	  </xsl:choose>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:if test="$length != ''">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'length'"/>
	<xsl:with-param name="arg"  select="$length"/>
	<xsl:with-param name="extra"  select="$extra"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:if test="$type = 'objectRef'">
      <xsl:call-template name="emit-statement">
        <xsl:with-param name="keyw" select="'require-instance'"/>
        <xsl:with-param name="arg">
          <xsl:choose>
            <xsl:when test="@exists">
              <xsl:value-of select="@exists"/>
            </xsl:when>
            <xsl:otherwise>
              <!-- Default in confspec is false, in YANG it is true -->
              <xsl:value-of select="'false'"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
	<xsl:with-param name="extra"  select="$extra"/>
      </xsl:call-template>
      <xsl:if test="@path_filters">
        <xsl:call-template name="emit-statement">
          <xsl:with-param name="keyw"  select="'tailf:path-filters'"/>
          <xsl:with-param name="arg"   select="@path_filters"/>
          <xsl:with-param name="extra" select="$extra"/>
        </xsl:call-template>
      </xsl:if>
    </xsl:if>

  </xsl:template>



  <xsl:template match="/">
    <xsl:text>// This file was generated by cs2yang.xsl</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates select="*|comment()"/>
  </xsl:template>

  <!--
     - Top level confspec tag
    -->
  <xsl:template match="cs:confspec">

    <!--
       - // header information
       - <yang-version statement>
       - <namespace statement>
       - <prefix statement>
    -->
    <xsl:choose>
      <xsl:when test="(@mount != '/') and (not(@mountNamespace) or
		      (@mountNamespace = @targetNamespace))">
	<xsl:call-template name="emit-submodule-block"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="emit-module-block"/>
      </xsl:otherwise>
    </xsl:choose>

    <!-- translate relevant top-level attributes -->
    <xsl:for-each select="@*">
      <xsl:choose>
	<xsl:when test="name() = 'allowEnumConflicts'"/>
	<xsl:when test="name() = 'targetNamespace'"/>
	<xsl:when test="name() = 'prefix'"/>
	<xsl:when test="name() = 'yangModuleName'"/>
	<xsl:when test="name() = 'id'"/>
	<xsl:when test="name() = 'mount'"/>
	<xsl:when test="name() = 'mountNamespace'"/>
	<xsl:when test="name() = 'version'"/>
	<xsl:otherwise>
	  <xsl:call-template name="translate-attribute">
	    <xsl:with-param name="name" select="name()"/>
	    <xsl:with-param name="value" select="."/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>

    <xsl:value-of select="$newline"/>

    <!--
       - // linkage statements
       - <import statements>
       - <include statements>
    -->
    
    <!-- we don't know which of these modules are actually needed, so
       import everything that could be needed -->
    <xsl:call-template name="emit-import-stmt">
      <xsl:with-param name="name" select="'ietf-yang-types'"/>
      <xsl:with-param name="prefix" select="'yang'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-import-stmt">
      <xsl:with-param name="name" select="'ietf-inet-types'"/>
      <xsl:with-param name="prefix" select="'inet'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-import-stmt">
      <xsl:with-param name="name" select="'tailf-common'"/>
      <xsl:with-param name="prefix" select="'tailf'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-import-stmt">
      <xsl:with-param name="name" select="'tailf-xsd-types'"/>
      <xsl:with-param name="prefix" select="'xs'"/>
    </xsl:call-template>

    <!-- Now take a shot at figuring out what else needs to be imported -->
    <xsl:variable name="module-ns">
      <xsl:call-template name="module-namespace"/>
    </xsl:variable>
    <xsl:for-each select="namespace::*">
      <xsl:variable name="ns" select="string(.)"/>
      <xsl:variable name="ns-pfx" select="name(.)"/>
      <xsl:choose>
	<xsl:when test="$ns-pfx = ''"/>
	<xsl:when test="$ns = ''"/>
	<xsl:when test="$ns = 'http://www.w3.org/XML/1998/namespace'"/>
	<xsl:when test="$ns = $ns-xs"/>
	<xsl:when test="$ns = $ns-confd"/>
	<xsl:when test="$ns = 'http://tail-f.com/ns/confspec/1.0'"/>
	<xsl:when test="$ns = $ns-smi"/>
	<xsl:when test="$ns = $module-ns"/>
	<xsl:when test="$ns = 'http://tail-f.com/ns/mibs/RFC-1212/1.0'"/>
	<xsl:when test="$ns = 'http://tail-f.com/ns/mibs/RFC-1155/1.0'"/>
	<xsl:when test="$ns = 'http://tail-f.com/ns/mibs/RFC-1215/1.0'"/>
	<xsl:otherwise>
	  <xsl:value-of select="concat('  /* ',$ns,' */',$newline)"/>
	  <xsl:variable name="component"
			select="str:tokenize($ns, '/')[last()-1]"/>
	  <xsl:variable name="filename">
	    <xsl:choose>
	      <xsl:when test="$component != ''">
		<xsl:value-of select="$component"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="$ns-pfx"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:variable>
	  <xsl:call-template name="emit-import-stmt">
	    <xsl:with-param name="name"   select="$filename"/>
	    <xsl:with-param name="prefix" select="$ns-pfx"/>
	    <xsl:with-param name="extra" select="-2"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>

    <!-- Import namespace we are mounting into -->
    <xsl:if test="@mountNamespace and (@mountNamespace != @targetNamespace)">
      <xsl:variable name="prefix">
	<xsl:call-template name="namespace-to-prefix">
	  <xsl:with-param name="ns" select="@mountNamespace"/>
	</xsl:call-template>
      </xsl:variable>
      <xsl:text>  /* cs2yang:
   * - Augmenting namespace: </xsl:text>
      <xsl:value-of select="concat(@mountNamespace, $newline)"/>
      <xsl:text>   * - The module name &quot;</xsl:text>
      <xsl:value-of select="$prefix"/>
      <xsl:text>&quot; is a guess</xsl:text>
      <xsl:value-of select="concat($newline,'   */',$newline)"/>
      <xsl:call-template name="emit-import-stmt">
	<xsl:with-param name="name" select="$prefix"/>
	<xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:value-of select="$newline"/>

    <!--
       - // meta information
       - <organization statement>
       - <contact statement>
       - <description statement>
       - <reference statement>
    -->
    <xsl:if test="cs:desc">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'description'"/>
	<xsl:with-param name="arg"  select="cs:desc/text()"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
      <xsl:value-of select="$newline"/>
    </xsl:if>


    <!--
       - // revision history
       - <revision statements>
    -->

    <!-- Throw out a revision statement if there is a version attribute -->
    <xsl:if test="@version and (string-length(@version) >= '8')">
      <xsl:variable name="rev-str">
	<xsl:value-of select="concat(substring(@version,1,4),'-',
			             substring(@version,5,2),'-',
				     substring(@version,7,2))"/>
      </xsl:variable>
      <xsl:value-of select="concat('  /* REVISION ',$QuoteChar,@version,
			           $QuoteChar,' */',$newline)"/>
      <xsl:call-template name="emit-statement-block">
	<xsl:with-param name="keyw" select="'revision'"/>
	<xsl:with-param name="arg"  select="$rev-str"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'description'"/>
	<xsl:with-param name="extra" select="'4'"/>
      </xsl:call-template>
      <xsl:call-template name="emit-close-block">
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
      <xsl:value-of select="$newline"/>
    </xsl:if>

    <!-- Translate a mount other than to / into an augment -->
    <xsl:if test="@mount != '/'">
      <xsl:variable name="path">
	<xsl:choose>
	  <xsl:when test="@mountNamespace and
			  (@mountNamespace != @targetNamespace)">
	    <xsl:variable name="prefix">
	      <xsl:call-template name="namespace-to-prefix">
		<xsl:with-param name="ns" select="@mountNamespace"/>
	      </xsl:call-template>
	    </xsl:variable>
	    <xsl:for-each select="str:tokenize(@mount, '/')">
	      <xsl:value-of select="concat('/',$prefix,':',.)"/>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@mount"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      
      <xsl:call-template name="emit-statement-block">
	<xsl:with-param name="keyw" select="'augment'"/>
	<xsl:with-param name="arg"  select="$path"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>

    <xsl:apply-templates select="*|comment()"/>

    <!-- end bracket for augment statement -->
    <xsl:if test="@mount != '/'">
      <xsl:call-template name="emit-close-block">
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>

    <!-- end bracket for module statement -->
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>


  <!-- keep comments (but don't bother differentiate between one
       and multi-line -->
  <xsl:template match="comment()">
    <xsl:call-template name="indent"/>
    <xsl:text>/* </xsl:text>
    <xsl:value-of select="."/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <!-- Turn <desc> anywhere into tailf:info (except at top-level) -->
  <xsl:template match="/cs:confspec/cs:desc"/>
  <xsl:template match="cs:desc">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:info'"/>
      <xsl:with-param name="arg" select="text()"/>
    </xsl:call-template>
  </xsl:template>



  <!--
     -  A somewhat simplistic approach to simpleType and its followers...
    -->

  <xsl:template match="xs:simpleType[child::xs:list]|
		       xs:simpleType[//xs:simpleType[(@name = current()/xs:restriction/@base) and child::xs:list]]">
    <xsl:text>/* WARNING cs2yang expands xs:list inline </xsl:text>
    <xsl:text>to a leaf-list in this file only.</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:text> * Any references to '</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>' in other files needs to be</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:text> * manually edited.</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:text>&lt;xs:simpleType name=&quot;</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>&quot;&gt;</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates select="*" mode="in-comment"/>
    <xsl:text>&lt;/simpleType&gt;</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="xs:simpleType">
    <xsl:if test="xs:restriction/xs:enumeration">
      <xsl:choose>
	<xsl:when test="contains(xs:restriction/@base, ':string')"/>
	<xsl:when test="contains(xs:restriction/@base, ':normalizedString')"/>
	<xsl:when test="contains(xs:restriction/@base, ':token')"/>
	<xsl:when test="contains(xs:restriction/@base, ':NMTOKEN')"/>
	<xsl:when test="contains(xs:restriction/@base, ':DisplayString')"/>
	<xsl:otherwise>
	  <xsl:text>/* WARNING cs2yang: YANG only supports </xsl:text>
	  <xsl:text>enumeration of string types. */
/* (Change the instrumentation code, or possibly use 'range' for numbers). */
</xsl:text>
	  <xsl:text>/* The original base type in the confspec was </xsl:text>
	  <xsl:value-of select="concat($QuoteChar,xs:restriction/@base,
				$QuoteChar)"/>
	  <xsl:text>. */
</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'typedef'"/>
      <xsl:with-param name="arg"  select="@name"/>
    </xsl:call-template>
    <!-- translate misplaced documentation -->
    <xsl:if test="xs:restriction/xs:annotation/xs:documentation">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'tailf:info'"/>
	<xsl:with-param name="arg" select="xs:restriction/xs:annotation/xs:documentation/text()"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <xsl:template match="xs:union[parent::xs:simpleType]">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'type'"/>
      <xsl:with-param name="arg"  select="'union'"/>
    </xsl:call-template>

    <xsl:for-each select="str:tokenize(@memberTypes)">
      <xsl:variable name="utype">
	<xsl:call-template name="translate-type">
	  <xsl:with-param name="type-str" select="."/>
	</xsl:call-template>
      </xsl:variable>
      <xsl:variable name="utype-implicit-restrictions">
	<xsl:call-template name="emit-type-implicit-restriction">
	  <xsl:with-param name="type-str" select="."/>
	  <xsl:with-param name="extra" select="'8'"/>
	</xsl:call-template>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="$utype-implicit-restrictions != ''">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'type'"/>
	    <xsl:with-param name="arg"  select="$utype"/>
	    <xsl:with-param name="blockp" select="true()"/>
	    <xsl:with-param name="quotep" select="false()"/>
	    <xsl:with-param name="extra" select="'6'"/>
	  </xsl:call-template>
	  <xsl:value-of select="$utype-implicit-restrictions"/>
	  <xsl:call-template name="emit-close-block">
	    <xsl:with-param name="extra" select="'6'"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="emit-statement-unquoted">
	    <xsl:with-param name="keyw" select="'type'"/>
	    <xsl:with-param name="arg" select="$utype"/>
	    <xsl:with-param name="extra" select="'6'"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <xsl:template match="cs:typepoint">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'type'"/>
      <xsl:with-param name="arg"  select="'string'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:typepoint'"/>
      <xsl:with-param name="arg"  select="@id"/>
    </xsl:call-template>
  </xsl:template>


  <xsl:template match="xs:restriction[parent::xs:simpleType]">
    <xsl:variable name="type">
      <xsl:choose>
	<xsl:when test="child::xs:enumeration">enumeration</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="translate-type">
	    <xsl:with-param name="type-str" select="@base"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'type'"/>
      <xsl:with-param name="arg"  select="$type"/>
      <xsl:with-param name="blockp" select="true()"/>
      <xsl:with-param name="quotep" select="false()"/>
    </xsl:call-template>

    <xsl:call-template name="emit-type-implicit-restriction">
      <xsl:with-param name="type-str" select="@base"/>
    </xsl:call-template>

    <xsl:if test="xs:pattern">
      <xsl:text> /* WARNING: patterns are expressed using W3C regular expressions in YANG */
</xsl:text>
      <xsl:text> /*          (In confspec POSIX regexps were used, check pattern below) */
</xsl:text>
    </xsl:if>

    <xsl:if test="count(xs:pattern) > 1">
        <xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw"  select="'pattern'"/>
	  <xsl:with-param name="arg">
	    <xsl:for-each select="xs:pattern">
	      <xsl:value-of select="@value"/>
	      <xsl:if test="following-sibling::xs:pattern">
		<xsl:text>|</xsl:text>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:with-param>
	  <xsl:with-param name="extra" select="'2'"/>
	</xsl:call-template>
    </xsl:if>

    <!-- Now recurse down through restrictions and enumerations -->
    <xsl:apply-templates select="*|comment()"/>

    <!-- finish of type declaration -->
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>


  <!-- Four combinations of min and maxInclusive -->
  <xsl:template match="xs:minInclusive[parent::xs:restriction and ../xs:maxInclusive]">
    <xsl:choose>
      <xsl:when test="@value = ../xs:maxInclusive/@value">
        <xsl:call-template name="emit-statement">
          <xsl:with-param name="keyw" select="'range'"/>
          <xsl:with-param name="arg"  select="@value"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="emit-statement">
          <xsl:with-param name="keyw" select="'range'"/>
          <xsl:with-param name="arg"  select="concat(@value, ' .. ', ../xs:maxInclusive/@value)"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="xs:minInclusive[parent::xs:restriction and not(../xs:maxInclusive)]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'range'"/>
      <xsl:with-param name="arg"  select="concat(@value, ' .. max')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="xs:maxInclusive[parent::xs:restriction and not(../xs:minInclusive)]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'range'"/>
      <xsl:with-param name="arg"  select="concat('min .. ', @value)"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="xs:maxInclusive[parent::xs:restriction and ../xs:minInclusive]"/>

  <!-- Combinations of min and maxLength -->
  <xsl:template match="xs:minLength[parent::xs:restriction and ../xs:maxLength]">
    <xsl:variable name="keyw">
      <xsl:call-template name="length-keyword">
        <xsl:with-param name="type-str" select="../@base"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="@value = ../xs:maxLength/@value">
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="$keyw"/>
	  <xsl:with-param name="arg"  select="@value"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="$keyw"/>
	  <xsl:with-param name="arg"  select="concat(@value, ' .. ', ../xs:maxLength/@value)"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="xs:minLength[parent::xs:restriction and not(../xs:maxLength)]">
    <xsl:variable name="keyw">
      <xsl:call-template name="length-keyword">
        <xsl:with-param name="type-str" select="../@base"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="$keyw"/>
      <xsl:with-param name="arg"  select="concat(@value, ' .. max')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="xs:maxLength[parent::xs:restriction and not(../xs:minLength)]">
    <xsl:variable name="keyw">
      <xsl:call-template name="length-keyword">
        <xsl:with-param name="type-str" select="../@base"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="$keyw"/>
      <xsl:with-param name="arg"  select="concat('min .. ', @value)"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="xs:maxLength[parent::xs:restriction and ../xs:minLength]"/>

  <!-- Enumerations -->
  <xsl:template match="xs:enumeration[parent::xs:restriction]">
    <xsl:choose>
      <xsl:when test="@idValue != ''">
	<xsl:call-template name="emit-statement-block">
	  <xsl:with-param name="keyw" select="'enum'"/>
	  <xsl:with-param name="arg"  select="@value"/>
	</xsl:call-template>
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="'value'"/>
	  <xsl:with-param name="arg"  select="@idValue"/>
	  <xsl:with-param name="extra" select="'2'"/>
	</xsl:call-template>
	<xsl:call-template name="emit-close-block"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="'enum'"/>
	  <xsl:with-param name="arg"  select="@value"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- length restriction -->
  <xsl:template match="xs:length[parent::xs:restriction]">
    <xsl:variable name="keyw">
      <xsl:call-template name="length-keyword">
        <xsl:with-param name="type-str" select="../@base"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw"  select="$keyw"/>
      <xsl:with-param name="arg"   select="@value"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
  </xsl:template>

  <!-- pattern restriction -->
  <xsl:template match="xs:pattern[parent::xs:restriction]">
    <xsl:variable name="ptype">
      <xsl:call-template name="translate-type">
	<xsl:with-param name="type-str" select="../@base"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="($ptype = 'string') or ($ptype = 'tailf:hex-list')">
	<!-- restrictions with more than one pattern are handled
	     explicitly in xs:restriction template -->
	<xsl:if test="count(../xs:pattern) = 1">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw"  select="'pattern'"/>
	    <xsl:with-param name="arg"   select="@value"/>
	  </xsl:call-template>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text> /* WARNING cs2yang: pattern </xsl:text>
	<xsl:value-of select="concat($QuoteChar,@value,$QuoteChar)"/>
	<xsl:text> is only legal for string type in YANG. */</xsl:text>
	<xsl:value-of select="$newline"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="xs:fractionDigits[parent::xs:restriction]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'xs:fraction-digits'"/>
      <xsl:with-param name="arg"  select="@value"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="xs:annotation[not(parent::xs:restriction)]">
    <xsl:apply-templates select="*|comment()"/>
  </xsl:template>

  <!-- skip misplaced documentation strings (they are handled
       explicitly in simpleType template) -->
  <xsl:template match="xs:annotation[parent::xs:restriction]"/>
  
  <!-- documentation strings in simpleType -->
  <xsl:template match="xs:documentation">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:info'"/>
      <xsl:with-param name="arg" select="text()"/>
    </xsl:call-template>
  </xsl:template>

  <!-- suppressEcho (is the only thing we have on xs:appinfo) -->
  <xsl:template match="xs:appinfo[@cs:suppressEcho]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:suppress-echo'"/>
      <xsl:with-param name="arg"  select="@cs:suppressEcho"/>
    </xsl:call-template>
  </xsl:template>

  <!-- seems confdc accepts it w/o proper prefix, we better do that too -->
  <xsl:template match="xs:appinfo[@suppressEcho]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:suppress-echo'"/>
      <xsl:with-param name="arg"  select="@suppressEcho"/>
    </xsl:call-template>
  </xsl:template>


  <!-- bitsType -->
  <xsl:template match="cs:bitsType">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'typedef'"/>
      <xsl:with-param name="arg"  select="@name"/>
    </xsl:call-template>

    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'type'"/>
      <xsl:with-param name="arg"  select="'bits'"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>

    <xsl:if test="(@size = '64') and not(child::cs:field[@bit > 32])">
      <xsl:text>/* WARNING cs2yang: will be interpreted as 32-bit field (no bit exceeds 32) */
</xsl:text>
    </xsl:if>

    <xsl:apply-templates select="*|comment()"/>

    <xsl:call-template name="emit-close-block">
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>
  
  <xsl:template match="cs:field[parent::cs:bitsType]">
    <xsl:call-template name="indent">
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
    <xsl:value-of select="concat('bit ', @label, ' { position ', @bit, '; } ',$newline)"/>
  </xsl:template>

  <!--structuredType -->
  <xsl:template match="cs:structuredType">
    <xsl:text>/* WARNING cs2yang will expand grouping correctly in this file only. </xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:text> * External users of '</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>' will have to change "leaf" with "type" to "container" with "uses" manually.</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="$newline"/>
    
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'grouping'"/>
      <xsl:with-param name="arg"  select="@name"/>
    </xsl:call-template>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>


  <!--
     - Callpoints and validation
    -->
  <xsl:template match="cs:callpoint|cs:validate">
    <xsl:variable name="extension">
      <xsl:choose>
	<!--
	<xsl:when test="@transform">
	  <xsl:value-of select="'transform'"/>
	</xsl:when>
	-->
	<xsl:when test="@type = 'cdb'">
	  <xsl:value-of select="'cdb-oper'"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="local-name()"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="concat('tailf:', $extension)"/>
      <xsl:with-param name="arg"  select="@id"/>
      <xsl:with-param name="blockp" select="true()"/>
      <xsl:with-param name="argp">
	<xsl:choose>
	  <xsl:when test="not(@id) or (@id = '') or (@type = 'cdb')">
	    <xsl:value-of select="false()"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="true()"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
	
    <!-- throw in attributes as statements for now... -->
    <xsl:for-each select="@*">
      <xsl:choose>
	<xsl:when test="name() = 'id'"/>
	<!-- <xsl:when test="name() = 'transform'"/> -->
	<xsl:when test="(name() = 'type') and (. = 'cdb')"/>
	<xsl:when test="(name() = 'type') and (. = 'internal')">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'tailf:internal'"/>
	    <xsl:with-param name="argp" select="false()"/>
	    <xsl:with-param name="extra" select="'2'"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="(name() = 'config') and (../@type = 'cdb')"/>
	<xsl:when test="name() = 'type'"/>
	<xsl:when test="name() = 'callOnce'">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'tailf:call-once'"/>
	    <xsl:with-param name="arg"  select="."/>
	    <xsl:with-param name="extra" select="'2'"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
          <xsl:variable name="translated-name">
            <xsl:choose>
              <xsl:when test="name() = 'cache'">
                <xsl:value-of select="'tailf:cache'"/>
              </xsl:when>
              <xsl:when test="name() = 'setHook'">
                <xsl:value-of select="'tailf:set-hook'"/>
              </xsl:when>
              <xsl:when test="name() = 'transactionHook'">
                <xsl:value-of select="'tailf:transaction-hook'"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="concat('tailf:',name())"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="$translated-name"/>
	    <xsl:with-param name="arg"  select="."/>
	    <xsl:with-param name="extra" select="'2'"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>

    <xsl:apply-templates select="*|comment()"/>

    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <xsl:template match="cs:dependency">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:dependency'"/>
      <xsl:with-param name="arg"  select="text()"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="cs:displayWhen">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:display-when'"/>
      <xsl:with-param name="arg"  select="@value"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="cs:secondaryIndex">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'tailf:secondary-index'"/>
      <xsl:with-param name="arg"  select="@name"/>
    </xsl:call-template>
    <xsl:if test="@indexElems">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'tailf:index-leafs'"/>
	<xsl:with-param name="arg"  select="@indexElems"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:if test="@sortOrder">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'tailf:sort-order'"/>
	<xsl:with-param name="arg">
	  <xsl:choose>
	    <xsl:when test="@sortOrder = 'snmp_implied'">
	      <xsl:text>snmp-implied</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@sortOrder"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:with-param>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!--
     - Actions
    -->
  <xsl:template match="cs:action">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'tailf:action'"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <xsl:template match="cs:actionpoint[parent::cs:action]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:actionpoint'"/>
      <xsl:with-param name="arg" select="@id"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="cs:confirmText[parent::cs:action]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:confirm-text'"/>
      <xsl:with-param name="arg" select="text()"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="cs:params[parent::cs:action]|cs:result[parent::cs:action]">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw">
	<xsl:choose>
	  <xsl:when test="name() = 'params'">
	    <xsl:text>input</xsl:text>
	  </xsl:when>
	  <xsl:when test="name() = 'result'">
	    <xsl:text>output</xsl:text>
	  </xsl:when>
	</xsl:choose>
      </xsl:with-param>
      <xsl:with-param name="blockp" select="true()"/>
      <xsl:with-param name="argp" select="false()"/>
    </xsl:call-template>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>


  <!-- Notification -->
  <xsl:template match="cs:notification">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'notification'"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>


  <!--
     - Now elems, lists, containers and leafs
    -->

  <!-- Special case, an elem without attributes is just a wrapper, recurse -->
  <xsl:template match="cs:elem[not(@*)]">
    <xsl:apply-templates select="*|comment()"/>
  </xsl:template>

  <!-- An untyped elem with key children is a list -->
  <xsl:template match="cs:elem[not(@type) and not(@symlink) and ((child::cs:elem[@key]) or (@maxOccurs > 1))]">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'list'"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>

    <!-- key declaration -->
    <xsl:if test="child::cs:elem[@key]">
      <xsl:call-template name="emit-statement-nodenames">
	<xsl:with-param name="keyw" select="'key'"/>
	<xsl:with-param name="arg"  select="child::cs:elem[@key]"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>
    <!-- unique declaration -->
    <xsl:if test="descendant::cs:elem[@unique]">
      <xsl:variable name="me" select="."/>
      <!-- select all descendants who has the unique attribute, and
	   whose first 'list' ancestor is me -->
      <xsl:variable name="allU"
		    select="descendant::cs:elem[@unique][generate-id(ancestor::cs:elem[not(@type) and not(@symlink) and ((child::cs:elem[@key]) or (@maxOccurs > 1))][1]) = generate-id($me)]"/>
      <!-- iterate over, output a unique statement for each group -->
      <xsl:for-each select="$allU">
        <xsl:sort select="@unique"/>
        <xsl:variable name="upos" select="position()"/>
        <xsl:if test="@unique != string($allU[$upos - 1]/@unique)">
          <xsl:variable name="cur" select="@unique"/>
          <xsl:call-template name="emit-statement-nodenames-relpath">
            <xsl:with-param name="keyw" select="'unique'"/>
            <xsl:with-param name="arg"  select="$allU[@unique = $cur]"/>
            <xsl:with-param name="parent" select="$me"/>
          </xsl:call-template>
        </xsl:if>
      </xsl:for-each>
    </xsl:if>
    <!-- indexedView element - always child of first&only key -->
    <xsl:if test="./cs:elem[1]/cs:indexedView">
      <xsl:call-template name="emit-statement">
        <xsl:with-param name="keyw" select="'tailf:indexed-view'"/>
        <xsl:with-param name="argp" select="false()"/>
        <xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>

    <!-- translate relevant attributes -->
    <xsl:for-each select="@*">
      <xsl:choose>
	<xsl:when test="(name() = 'minOccurs') and (. != 0)">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'min-elements'"/>
	    <xsl:with-param name="arg"  select="."/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="name() = 'minOccurs'"/>
	<xsl:when test="(name() = 'maxOccurs') and (. != 'unbounded')">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'max-elements'"/>
	    <xsl:with-param name="arg"  select="."/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="name() = 'maxOccurs'"/>
	<xsl:otherwise>
	  <xsl:call-template name="translate-attribute">
	    <xsl:with-param name="name" select="name()"/>
	    <xsl:with-param name="value" select="."/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>

    <xsl:apply-templates select="*|comment()"/>

    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!-- An untyped elem w children (but without key children) is a container -->
  <xsl:template match="cs:elem[not(@type) and @name and not(@symlink) and (child::cs:elem|child::cs:action|child::cs:choice) and not(child::cs:elem[@key]) and not(@maxOccurs > 1)]">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'container'"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>

    <!-- translate relevant attributes -->
    <xsl:for-each select="@*">
      <xsl:choose>
	<xsl:when test="name() = 'maxOccurs'"/>
	<xsl:when test="(name() = 'minOccurs') and (. = 0)">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'presence'"/>
	    <xsl:with-param name="arg"  select="''"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="name() = 'minOccurs'"/>
	<xsl:otherwise>
	  <xsl:call-template name="translate-attribute">
	    <xsl:with-param name="name" select="name()"/>
	    <xsl:with-param name="value" select="."/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!-- An elem without elem children is a leaf or leaf-list -->
  <xsl:template match="cs:elem[@name and not(child::cs:elem|child::cs:action|child::cs:choice) and not(@symlink)]">

    <xsl:variable name="typeName" select="@type"/>

    <!-- Is this a locally declared leaf-list, and if so of what type? -->
    <xsl:variable name="listTypeName">
      <xsl:choose>
	<xsl:when test="not($typeName)"/>
	<xsl:when test="starts-with($typeName, 'xs:')"/>
	<xsl:when test="starts-with($typeName, 'confd:')"/>

	<!-- should be /cs:confspec/xs:simpleType, but then cs2yang -w
	   - won't work :-(
	-->
	<xsl:when test="//xs:simpleType[@name = $typeName]/xs:list">
	  <xsl:value-of select="//xs:simpleType[@name = $typeName]/xs:list/@itemType"/>
	</xsl:when>
	<xsl:when test="//xs:simpleType[@name = (//xs:simpleType[@name = $typeName]/xs:restriction/@base)]/xs:list">
	  <xsl:value-of select="//xs:simpleType[@name = (//xs:simpleType[@name = $typeName]/xs:restriction/@base)]/xs:list/@itemType"/>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="structTypeName">
      <xsl:choose>
	<xsl:when test="not($typeName)"/>
	<xsl:when test="starts-with($typeName, 'xs:')"/>
	<xsl:when test="starts-with($typeName, 'confd:')"/>
	<xsl:when test="//cs:structuredType[@name = $typeName]">
	  <xsl:value-of select="$typeName"/>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="keyw">
      <xsl:choose>
	<xsl:when test="$listTypeName != ''">
	  <xsl:text>leaf-list</xsl:text>
	</xsl:when>
	<xsl:when test="$structTypeName != ''">
	  <xsl:text>container</xsl:text>
	</xsl:when>
	<xsl:when test="@maxOccurs > 1">
	  <xsl:text>leaf-list</xsl:text>
	</xsl:when>
	<xsl:when test="@maxOccurs = 'unbounded'">
	  <xsl:text>leaf-list</xsl:text>
	</xsl:when>
	<xsl:when test="@isLeafList = 'true'">
	  <xsl:text>leaf-list</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>leaf</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="$keyw"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>

    <!-- Type declaration -->
    <xsl:choose>
      <xsl:when test="$structTypeName != ''">
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="'uses'"/>
	  <xsl:with-param name="arg" select="$structTypeName"/>
	  <xsl:with-param name="extra" select="'2'"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="@keyref">
	<xsl:if test="preceding-sibling::cs:elem[@keyref]">
	  <xsl:text>/* FIXME cs2yang: the following leafref needs to have predicates added to have the same meaning as in the confspec keyref */</xsl:text>
          <xsl:value-of select="$newline"/>
	</xsl:if>
	<xsl:call-template name="emit-statement-block">
	  <xsl:with-param name="keyw" select="'type'"/>
	  <xsl:with-param name="arg"  select="'leafref'"/>
	  <xsl:with-param name="extra" select="'2'"/>
	</xsl:call-template>
	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="'path'"/>
	  <xsl:with-param name="arg"  select="@keyref"/>
	  <xsl:with-param name="extra" select="'4'"/>
	</xsl:call-template>
	<xsl:call-template name="emit-close-block">
	  <xsl:with-param name="extra" select="'2'"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="thisType">
	  <xsl:choose>
	    <xsl:when test="$listTypeName != ''">
	      <xsl:value-of select="$listTypeName"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@type"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<xsl:if test="@defaultRef">
	  <xsl:text>/* FIXME cs2yang: this type is probably wrong, it should be the same type as </xsl:text>
	  <xsl:value-of select="concat(@defaultRef, ' */', $newline)"/>
	</xsl:if>

	<xsl:variable name="implicit-restriction">
	  <xsl:call-template name="emit-type-implicit-restriction">
	    <xsl:with-param name="type-str" select="$thisType"/>
	    <xsl:with-param name="extra" select="'4'"/>
	  </xsl:call-template>
	</xsl:variable>

	<xsl:call-template name="emit-statement">
	  <xsl:with-param name="keyw" select="'type'"/>
	  <xsl:with-param name="arg">
	    <xsl:call-template name="translate-type">
	      <xsl:with-param name="type-str" select="$thisType"/>
	    </xsl:call-template>
	  </xsl:with-param>
	  <xsl:with-param name="quotep" select="false()"/>
	  <xsl:with-param name="blockp" select="$implicit-restriction != ''"/>
	  <xsl:with-param name="extra" select="'2'"/>
	</xsl:call-template>

	<xsl:if test="$implicit-restriction != ''">
	  <xsl:value-of select="$implicit-restriction"/>
	  <xsl:call-template name="emit-close-block">
	    <xsl:with-param name="extra" select="'2'"/>
	  </xsl:call-template>
	</xsl:if>
	
      </xsl:otherwise>
    </xsl:choose>

    <!-- If it was a leaf-list, check if it has restrictions -->
    <xsl:if test="$keyw = 'leaf-list'">
      <xsl:if test="//xs:simpleType[@name = (//xs:simpleType[@name = $typeName]/xs:restriction/@base)]">
	<xsl:for-each select="//xs:simpleType[@name = $typeName]/xs:restriction/*">
	  <xsl:choose>
	    <xsl:when test="(local-name() = 'minLength') or
			    (local-name() = 'maxLength')">
	      <xsl:call-template name="emit-statement">
		<xsl:with-param name="keyw">
		  <xsl:choose>
		    <xsl:when test="local-name() = 'minLength'">
		      <xsl:text>min-elements</xsl:text>
		    </xsl:when>
		    <xsl:when test="local-name() = 'maxLength'">
		      <xsl:text>max-elements</xsl:text>
		    </xsl:when>
		    <xsl:otherwise><xsl:value-of select="name()"/></xsl:otherwise>
		  </xsl:choose>
		</xsl:with-param>
		<xsl:with-param name="arg" select="@value"/>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>/* FIXME cs2yang: couldn't translate:</xsl:text>
	      <xsl:value-of select="$newline"/>
	      <xsl:apply-templates select="." mode="in-comment"/>
	      <xsl:text>*/</xsl:text>
	      <xsl:value-of select="$newline"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:if>
    </xsl:if>

    <!-- mandatory? -->
    <xsl:if test="not(@key) and (not(@minOccurs) or @minOccurs = '1') and not(@default) and not(@defaultRef) and ($listTypeName = '') and ($structTypeName = '')">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'mandatory'"/>
	<xsl:with-param name="arg"  select="'true'"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>

    <!-- all other attributes -->
    <xsl:for-each select="@*">
      <xsl:choose>
	<xsl:when test="name() = 'type'"/>
	<xsl:when test="name() = 'key'"/>
	<xsl:when test="name() = 'keyref'"/>
	<xsl:when test="name() = 'unique'"/>
	<xsl:when test="name() = 'isLeafList'"/>
	<xsl:when test="(name() = 'default') and ($keyw = 'leaf-list')">
	  <xsl:text>/* FIXME cs2yang: leaf-list can not have </xsl:text>
	  <xsl:text>default value. Ignoring default=</xsl:text>
	  <xsl:value-of
	     select="concat($QuoteChar,.,$QuoteChar,' */',$newline)"/>
	</xsl:when>
	<xsl:when test="(name() = 'maxOccurs') and ($keyw = 'leaf-list')">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'max-elements'"/>
	    <xsl:with-param name="arg"  select="."/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="name() = 'maxOccurs'"/>
	<xsl:when test="(name() = 'minOccurs') and (. > 0) and ($keyw = 'leaf-list')">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'min-elements'"/>
	    <xsl:with-param name="arg"  select="."/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="($structTypeName != '') and
			(name() = 'minOccurs') and (. = 0)">
	  <xsl:call-template name="emit-statement">
	    <xsl:with-param name="keyw" select="'presence'"/>
	    <xsl:with-param name="arg"  select="''"/>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="name() = 'minOccurs'"/>
	<xsl:otherwise>
	  <xsl:call-template name="translate-attribute">
	    <xsl:with-param name="name" select="name()"/>
	    <xsl:with-param name="value" select="."/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>

    <!-- Descriptions, callpoints... -->
    <xsl:apply-templates select="*|comment()"/>

    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!-- symlink -->
  <xsl:template match="cs:elem[@symlink]">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'tailf:symlink'"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'tailf:path'"/>
      <xsl:with-param name="arg" select="@symlink"/>
      <xsl:with-param name="extra" select="'2'"/>
    </xsl:call-template>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!-- Officially we don't support choice in confspec, but we have
     -  some tests with them, so why not...
    -->
  <xsl:template match="cs:choice|cs:case">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="local-name()"/>
      <xsl:with-param name="arg" select="@name"/>
    </xsl:call-template>
    <!-- mandatory? -->
    <xsl:if test="cs:choice and (not(@minOccurs) or @minOccurs = '1') and not(@default)">
      <xsl:call-template name="emit-statement">
	<xsl:with-param name="keyw" select="'mandatory'"/>
	<xsl:with-param name="arg"  select="'true'"/>
	<xsl:with-param name="extra" select="'2'"/>
      </xsl:call-template>
    </xsl:if>
    <!-- take care of attributes -->
    <xsl:for-each select="@*">
      <xsl:choose>
	<xsl:when test="name() = 'minOccurs'"/>
	<xsl:when test="name() = 'maxOccurs'"/>
	<xsl:otherwise>
	  <xsl:call-template name="translate-attribute">
	    <xsl:with-param name="name" select="name()"/>
	    <xsl:with-param name="value" select="."/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!-- indexedView is already handled -->
  <xsl:template match="cs:indexedView"/>

  <!--
     - Take care of annotations as well
    -->
  <xsl:template match="csa:annotations">
    <xsl:call-template name="emit-module-block"/>
    <!--
    <xsl:call-template name="emit-import-stmt">
      <xsl:with-param name="name" select="'tailf-extensions'"/>
      <xsl:with-param name="prefix" select="'tailf'"/>
    </xsl:call-template>
    -->
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <xsl:template match="csa:modify">
    <xsl:call-template name="emit-statement-block">
      <xsl:with-param name="keyw" select="'tailf:annotate'"/>
      <xsl:with-param name="arg" select="csa:path/text()"/>
    </xsl:call-template>
    <xsl:apply-templates select="*|comment()"/>
    <xsl:call-template name="emit-close-block"/>
  </xsl:template>

  <!-- same thing for must... -->
  <xsl:template match="cs:must">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw" select="'must'"/>
      <xsl:with-param name="arg"  select="@value"/>
      <xsl:with-param name="blockp" select="boolean(child::*)"/>
    </xsl:call-template>
    <xsl:if test="child::*">
      <xsl:apply-templates select="*|comment()"/>
      <xsl:call-template name="emit-close-block"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="cs:errorMessage|cs:errorAppTag">
    <xsl:call-template name="emit-statement">
      <xsl:with-param name="keyw">
	<xsl:choose>
	  <xsl:when test="name() = 'errorMessage'">
	    <xsl:text>error-message</xsl:text>
	  </xsl:when>
	  <xsl:when test="name() = 'errorAppTag'">
	    <xsl:text>error-app-tag</xsl:text>
	  </xsl:when>
	</xsl:choose>
      </xsl:with-param>
      <xsl:with-param name="arg" select="text()"/>
    </xsl:call-template>
  </xsl:template>



  <xsl:template match="csa:path"/>

  <!-- FIXME:
       csa:symlink
       csa:indexedView
    -->
  <xsl:template match="csa:*">
    <xsl:call-template name="translate-attribute">
      <xsl:with-param name="name" select="local-name()"/>
      <xsl:with-param name="value" select="./text()"/>
    </xsl:call-template>
  </xsl:template>



  <!--
     - Throw in a copy of stuff I didn't take care of
    -->
  <xsl:template match="*">
    <!-- So why doesn't copy-of work if output mode isn't xml? -->
    <xsl:text>/* FIXME cs2yang: couldn't translate:</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates select="." mode="in-comment"/>
    <xsl:text>*/</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="*" mode="in-comment">
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="name()"/>
    <xsl:for-each select="@*">
      <xsl:value-of select="concat(' ', name(),'=')"/>
      <xsl:text>&quot;</xsl:text>
      <xsl:value-of select="."/>
      <xsl:text>&quot;</xsl:text>
    </xsl:for-each>
    <xsl:text>&gt;</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates select="*" mode="in-comment"/>
    <xsl:text>&lt;/</xsl:text>
    <xsl:value-of select="name()"/>
    <xsl:text>&gt;</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <!-- debugging -->
  <xsl:template match="text()">
    <xsl:text>/* Skipped text:
</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>*/
</xsl:text>
  </xsl:template>


</xsl:stylesheet>
