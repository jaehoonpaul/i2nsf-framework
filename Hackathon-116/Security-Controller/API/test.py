import xml.etree.ElementTree as ET
xml = """<i2nsf-cfi-policy>
  <name>policy-2</name>
  <rules>
    <action>
      <primary-action>
        <action>drop</action>
      </primary-action>
    </action>
    <condition>
      <ddos>
        <rate-limit>
          <packet-rate-threshold>1000</packet-rate-threshold>
        </rate-limit>
      </ddos>
    </condition>
    <name>rule1</name>
  </rules>
</i2nsf-cfi-policy>"""

tree = ET.fromstring(xml)
# print(tree.tag)
# for table in tree.iter(tree.tag):
#     for child in table:
#         print(child.tag)
#         for table2 in tree.iter(child.tag):
#             for child2 in table2:
#                 print(child2.tag)

def readXML(tree,tag,xpath,result):
  for table in tree.iter(tag):
      for child in table:
          if child.text[0] == "\n":
              xpath += f"/{child.tag}"
              readXML(tree,child.tag,xpath,result)
          else:
              result[f"{xpath}/{child.tag}"]= f"{child.text}"
  return result

highData = readXML(tree,tree.tag,f"/{tree.tag}",{})
print(highData)
