import xmltodict
import xml.etree.ElementTree as ET

tree = ET.parse('logs.xml')
root = tree.getroot()

#print(root)

for child in root:
	print(child.text)


s = open("logs.xml","r")

#print(xmltodict.parse(s))

data = xmltodict.parse(s)

print(data['notification']['i2nsf-nsf-detection-ddos']['attack-src-ip'])

xmldata = """<i2nsf-cfi-policy:policy>
	<endpoint-group>
		<user-group>
			<name>ddos-attacker</name>
			<ipv4>10.0.0.37</ipv4>
		</user-group>
	</endpoint-group>
</ietf-i2nsf-cfi-policy:policy>
"""

f = open("feedback_endpoint.xml", "w")
f.write(xmldata)
