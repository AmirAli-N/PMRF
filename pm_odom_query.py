# coding=utf-8

import http.client
import urllib
import xml.dom.minidom


CT_HOST = 'geo.dot.ca.gov'
CT_PORT = 80
CT_ENDPT = '/pmws/services/PostmileWebService'

FIPS_CTY = {
      1:'ALA',   3:'ALP',   5:'AMA',   7:'BUT',   9:'CAL',  11:'COL',  13:'CC',   15:'DN',
     17:'ED',   19:'FRE',  21:'GLE',  23:'HUM',  25:'IMP',  27:'INY',  29:'KER',  31:'KIN',
     33:'LAK',  35:'LAS',  37:'LA',   39:'MAD',  41:'MRN',  43:'MPA',  45:'MEN',  47:'MER',
     49:'MOD',  51:'MNO',  53:'MON',  55:'NAP',  57:'NEV',  59:'ORA',  61:'PLA',  63:'PLU',
     65:'RIV',  67:'SAC',  69:'SBT',  71:'SBD',  73:'SD',   75:'SF',   77:'SJ',   79:'SLO',
     81:'SM',   83:'SB',   85:'SCL',  87:'SCR',  89:'SHA',  91:'SIE',  93:'SIS',  95:'SOL',
     97:'SON',  99:'STA', 101:'SUT', 103:'TEH', 105:'TRI', 107:'TUL', 109:'TUO', 111:'VEN',
    113:'YOL', 115:'YUB'
}

class Postmile(object):
    def __init__(self, cty, rtnum, rtsfx, pmpfx, pmval, pmsfx, alignment, aligncode):
        if cty is not None:
            if type(cty) is not str:
                raise ValueError('Invalid parameter: cty')
        if ((rtnum is not None) and (type(rtnum) is not int)):
            raise ValueError('Invalid parameter: rtnum')
        if ((rtsfx is not None) and (type(rtsfx) is not str)):
            raise ValueError('Invalid parameter: rtsfx')
        if ((pmpfx is not None) and (type(pmpfx) is not str)):
            raise ValueError('Invalid parameter: pmpfx')
        if (type(pmval) is int):
            pmval = float(pmval)
        if ((pmval is not None) and (type(pmval) is not float)):
            raise ValueError('Invalid parameter: pmval')
        if ((pmsfx is not None) and (type(pmsfx) is not str)):
            raise ValueError('Invalid parameter: pmsfx')
        if ((alignment is not None) and (type(alignment) is not str)):
            raise ValueError('Invalid parameter: alignment')
        if ((aligncode is not None) and (type(aligncode) is not str)):
            raise ValueError('Invalid parameter: aligncode')
        self.cty = cty
        self.rtnum = rtnum
        self.rtsfx = rtsfx
        self.pmpfx = pmpfx
        self.pmval = pmval
        self.pmsfx = pmsfx
        self.alignment = alignment
        self.aligncode = aligncode
    def __str__(self):
        return ('['
            + str(self.cty)       + ","
            + str(self.rtnum)     + ","
            + str(self.rtsfx)     + ","
            + str(self.pmpfx)     + ","
            + str(self.pmval)     + ","
            + str(self.pmsfx)     + ","
            + str(self.alignment) + ","
            + str(self.aligncode)
            + ']'
        )

class SoapParseError(Exception):
    def __init__(self, msg):
        self.msg = msg
    def __str__(self):
        return repr(self.msg)

# Get postmile(s) from an odometer value.
#
# Parameters:
#   rtnum (int):          the route number or None to not specify
#   rtsfx (str/None):     the route suffix or None to not specify
#   odomval (float):      the odometer value
#   alignment (str/None): the route alignment or None to not specify
#
# Returns: (Postmile[]): a list of matching Postmile objects
#
def getpmfromodom_caltrans(rtnum, rtsfx, odomval, alignment):
    resp = submitGetPostmileForOdometerQuery(rtnum, rtsfx, odomval, alignment)
    result = parseGetPostmileForOdometerResult(resp, alignment)
    return result

# Get odometer value(s) from a postmile.
# NOTE: this Caltrans call does not allow specification of pmsfx.
#
# Parameters:
#   cty (str):            the county or None to not specify
#   rtnum (int):          the route number or None to not specify
#   rtsfx (str/None):     the route suffix or None to not specify
#   pmpfx (str/None):     the postmile prefix or None to not specify
#   pmval (float):        the postmile scalar value
#   alignment (str/None): the route alignment or None to not specify
#
# Returns: (float[]): a list of matching odometer values
#
def getodomfrompm_caltrans(cty, rtnum, rtsfx, pmpfx, pmval, alignment):
    resp = submitGetOdometerForPostmileQuery(cty, rtnum, rtsfx, pmpfx, pmval, alignment)
    result = parseGetOdometerForPostmileResult(resp)
    return result

# Make a getOdometerForPostmile call to the Caltrans service.
#
# Parameters:
#   cty (str):            the county or None if not specified
#   rtnum (int):          the route number or None if not specified
#   rtsfx (str/None):     the route suffix or None if not specified
#   pmpfx (str/None):     the postmile prefix or None if not specified
#   pmval (float):        the postmile value
#   alignment (str/None): the route alignment or None if not specified
#
# Returns:
#   (str): the HTTP response
#
def submitGetOdometerForPostmileQuery(cty, rtnum, rtsfx, pmpfx, pmval, alignment):
    if type(cty) is not str:
        raise ValueError('Invalid parameter: cty')
    if type(rtnum) is not int:
        raise ValueError('Invalid parameter: rtnum')
    if ((rtsfx is not None) and (type(rtsfx) is not str)):
        raise ValueError('Invalid parameter: rtsfx')
    if ((pmpfx is not None) and (type(pmpfx) is not str)):
        raise ValueError('Invalid parameter: pmpfx')
    if (type(pmval) is int):
        pmval = float(pmval)
    if type(pmval) is not float:
        raise ValueError('Invalid parameter: pmval')
    if ((alignment is not None) and (type(alignment) is not str)):
        raise ValueError('Invalid parameter: alignment')
    body = (
        '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"\n'
        '               xmlns:q0="urn:webservice.postmile.lrs.gis.dot.ca.gov"\n'
        '               xmlns:xsd="http://www.w3.org/2001/XMLSchema"\n'
        '               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">\n'
        '  <soap:Body>\n'
        '    <q0:getOdometerForPostmileParameters>\n'
    )
    if (alignment is not None):
        body += '    <q0:alignment>' + xmlElemEsc(alignment) + '</q0:alignment>\n'
    else:
        body += '    <q0:alignment xsi:nil="true" />\n'
    body += (
        '      <q0:postmile>\n'
    )
    # NOTE: The element is named "alignmentCode", but this is not the same thing as
    # "alignCode" elsewhere (e.g., in the Caltrans PM database or in the CAPM interface);
    # the Caltrans interface wants "L" or "R" here.
    if (alignment is not None):
        body += '        <q0:alignmentCode>' + xmlElemEsc(alignment) + '</q0:alignmentCode>\n'
    else:
        body += '        <q0:alignmentCode xsi:nil="true" />\n'
    if (cty is not None):
        body += '        <q0:countyCode>' + xmlElemEsc(cty) + '</q0:countyCode>\n'
    else:
        body += '        <q0:countyCode xsi:nil="true" />\n'
    if (pmpfx is not None):
        body += '        <q0:postmilePrefixCode>' + xmlElemEsc(pmpfx) + '</q0:postmilePrefixCode>\n'
    else:
        body += '        <q0:postmilePrefixCode xsi:nil="true" />\n'
    body +=     '        <q0:postmileValue>' + xmlElemEsc(pmval) + '</q0:postmileValue>\n'
    if (rtnum is not None):
        body += '        <q0:routeNumber>' + xmlElemEsc(rtnum) + '</q0:routeNumber>\n'
    else:
        body += '        <q0:routeNumber xsi:nil="true" />\n'
    if (rtsfx is not None):
        body += '        <q0:routeSuffixCode>' + xmlElemEsc(rtsfx) + '</q0:routeSuffixCode>\n'
    else:
        body += '        <q0:routeSuffixCode xsi:nil="true" />\n'
    body += (
        '      </q0:postmile>\n'
        '    </q0:getOdometerForPostmileParameters>\n'
        '  </soap:Body>\n'
        '</soap:Envelope>\n'
    )
    headers = {
        'Host'           : CT_HOST,
        'Content-Type'   : 'text/xml',
        'Content-Length' : len(body),
        'SOAPAction'     : 'getOdometerForPostmile'
        }
    conn = http.client.HTTPConnection(CT_HOST, CT_PORT)
    conn.request('POST', CT_ENDPT, body, headers)
    response = conn.getresponse()
    data = response.read()
    conn.close()
    return data

# Handle a getOdometerForPostmile response from the Caltrans service.
#
# Parameters:
#   data (str): the HTTP response
# Returns: a list of matching odometer values
#
def parseGetOdometerForPostmileResult(data):
    if (data is None):
        raise ValueError('Invalid parameter: data')
    xmldoc = xml.dom.minidom.parseString(data)
    envElem = getFirstElem(xmldoc, 'soapenv:Envelope')
    bodyElem = getFirstElem(envElem, 'soapenv:Body')
    if not existsElem(envElem, 'getOdometerForPostmileReturn'):
        return []
    rtnElem = getFirstElem(bodyElem, 'getOdometerForPostmileReturn')
    str_odoms = getChildDataOfAll(rtnElem, 'odometer')
    odoms = []
    for str_odom in str_odoms:
        odom = float(str_odom)
        odoms.append(odom)
    xmldoc.unlink()
    return odoms

# Make a getPostmileForOdometer call to the Caltrans service.
#
# Parameters:
#   rtnum (int):          the route number
#   rtsfx (str/None):     the route suffix or None if not specified
#   odomval (float):      the odometer value
#   alignment (str/None): the route alignment or None if not specified
# Returns:
#   (str): the HTTP response
#
def submitGetPostmileForOdometerQuery(rtnum, rtsfx, odomval, alignment):
    if type(rtnum) is not int:
        raise ValueError('Invalid parameter: rtnum')
    if ((rtsfx is not None) and (type(rtsfx) is not str)):
        raise ValueError('Invalid parameter: rtsfx')
    if type(odomval) is not float:
        raise ValueError('Invalid parameter: odomval')
    if ((alignment is not None) and (type(alignment) is not str)):
        raise ValueError('Invalid parameter: alignment')
    body = (
        '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"\n'
        '               xmlns:q0="urn:webservice.postmile.lrs.gis.dot.ca.gov"\n'
        '               xmlns:xsd="http://www.w3.org/2001/XMLSchema"\n'
        '               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">\n'
        '  <soap:Body>\n'
        '    <q0:getPostmileForOdometerParameters>\n'
    )
    # NOTE: The element is named "routeAlignment", but this is not the same thing as
    # "alignCode" elsewhere (e.g., in the Caltrans PM database or in the CAPM interface);
    # the Caltrans interface wants "L" or "R" here.
    if (alignment is not None):
        body += '      <q0:routeAlignment>' + xmlElemEsc(alignment) + '</q0:routeAlignment>\n'
    else:
        body += '      <q0:routeAlignment xsi:nil="true" />\n'
    body +=     '      <q0:odometer>' + xmlElemEsc(odomval) + '</q0:odometer>\n'
    body +=     '      <q0:routeNumber>' + xmlElemEsc(rtnum) + '</q0:routeNumber>\n'
    if (rtsfx is not None):
        body += '      <q0:routeSuffixCode>' + xmlElemEsc(rtsfx) + '</q0:routeSuffixCode>\n'
    else:
        body += '      <q0:routeSuffixCode xsi:nil="true" />\n'
    body += (
        '    </q0:getPostmileForOdometerParameters>\n'
        '  </soap:Body>\n'
        '</soap:Envelope>\n'
    )
    headers = {
        'Host'           : CT_HOST,
        'Content-Type'   : 'text/xml',
        'Content-Length' : len(body),
        'SOAPAction'     : 'getPostmileForOdometer'
        }
    conn = http.client.HTTPConnection(CT_HOST, CT_PORT)
    conn.request('POST', CT_ENDPT, body, headers)
    response = conn.getresponse()
    data = response.read()
    conn.close()
    return data

# Parse the response from a getPostmileForOdometer call to the Caltrans service.
#
# Parameters:
#   data (str):        the HTTP response
#   qalign (str/None): the alignment parameter used in the query
#
# Returns: (Postmile[]): a list of matching postmiles
#
# NOTE: Due to limitations of the Caltrans API, the returned Postmile's
#       alignment field may be null.
#
def parseGetPostmileForOdometerResult(data, qalign):
    if (data is None):
        raise ValueError('Invalid parameter: data')
    xmldoc = xml.dom.minidom.parseString(data)
    nrp_x     = None
    nrp_y     = None
    nrp_z     = None
    nrp_m     = None
    nrp_srid  = None
    aligncode = None
    cty       = None
    rtnum     = None
    rtsfx     = None
    pmpfx     = None
    pmval     = None
    pmsfx     = None
    envElem = getFirstElem(xmldoc, 'soapenv:Envelope')
    bodyElem = getFirstElem(envElem, 'soapenv:Body')
    if not existsElem(bodyElem, 'getPostmileForOdometerReturn'):
        return []
    rtnElem  = getFirstElem(bodyElem, 'getPostmileForOdometerReturn')
    if not existsElem(rtnElem, 'postmile'):
        return []
    pms = []
    pmElems = getElems(rtnElem, 'postmile')
    for pmElem in pmElems:
        if (isNil(pmElem)):
            pms.append(None)
        aligncode = getChildData(pmElem, 'alignmentCode')
        cty       = getChildData(pmElem, 'countyCode')
        pmpfx     = getChildData(pmElem, 'postmilePrefixCode')
        pmval     = getChildData(pmElem, 'postmileValue')
        rtnum     = getChildData(pmElem, 'routeNumber')
        rtsfx     = getChildData(pmElem, 'routeSuffixCode')
        xmldoc.unlink()
        # kludge to work around the fact that the Caltrans API uses different
        # alignment/aligncode/pmsfx logic than the Caltrans roadway dataset does
        # NOTE: this still doesn't handle the case when qalign is None.
        # this makes alignment UNKNOWN, not NULL in these cases!
        # also, pmsfx codes besides L and R (e.g., X) aren't supported!
        alignment = None
        if (aligncode is not None):
            if (aligncode == 'L'):
                aligncode = 'Left Split Align'
                alignment = 'L'
                if pmsfx is None:
                    pmsfx = 'L'
            elif (aligncode == 'R'):
                aligncode = 'Right Split Align'
                alignment = 'R'
                if pmsfx is None:
                    pmsfx = 'R'
            else:
                aligncode = None
                alignment = None
        else:
            if (qalign == 'L'):
                aligncode = 'Left'
                alignment = 'L'
            elif (qalign == 'R'):
                aligncode = 'Right'
                alignment = 'R'
            else:
                alignment = None
                aligncode = None
        if (cty is not None):
            cty = str(cty)
        if (rtnum is not None):
            rtnum = int(rtnum)
        if (rtsfx is not None):
            rtsfx = str(rtsfx)
        if (pmpfx is not None):
            pmpfx = str(pmpfx)
        if (pmval is not None):
            pmval = float(pmval)
        if (pmsfx is not None):
            pmsfx = str(pmsfx)
        if (alignment is not None):
            alignment = str(alignment)
        if (aligncode is not None):
            aligncode = str(aligncode)
        pm = Postmile(cty, rtnum, rtsfx, pmpfx, pmval, pmsfx, alignment, aligncode)
        pms.append(pm)
    return pms

def xmlElemEsc(s):
    esc = str(s)
    # order is consequential
    esc = esc.replace('&', '&amp;')
    esc = esc.replace('>', '&gt;')
    esc = esc.replace('<', '&lt;')
    esc = esc.replace("'", '&apos;')
    esc = esc.replace('"', '&quot;')
    return esc;

def xmlAttrEsc(s):
    esc = str(s)
    esc = esc.replace('"', '&quot;')
    return esc;

def getElems(node, name):
    if (node is None):
        raise ValueError('node is None')
    elems = node.getElementsByTagName(name)
    return elems

def getFirstElem(node, name):
    if (node is None):
        raise ValueError('node is None')
    elems = node.getElementsByTagName(name)
    if (len(elems) < 1):
        raise SoapParseError('element "' + name + '" not found')
    return elems[0]

def existsElem(node, name):
    if (node is None):
        raise ValueError('node is None')
    elems = node.getElementsByTagName(name)
    if (len(elems) < 1):
        return False
    return True

def getFirstChildData(node):
    if (node is None):
        raise ValueError('node is None')
    if (len(node.childNodes) < 1):
        return None
    return node.childNodes[0].data

# returns as string, or None
def getChildData(parentElem, childName):
    if (parentElem is None):
        raise ValueError('parentElem is None')
    elem = getFirstElem(parentElem, childName)
    if (elem.getAttribute('xsi:nil') == 'true'):
        return None
    return getFirstChildData(elem)

# returns list
def getChildDataOfAll(parentElem, childName):
    if (parentElem is None):
        raise ValueError('parentElem is None')
    elems = getElems(parentElem, childName)
    childData = []
    for elem in elems:
        if (elem.getAttribute('xsi:nil') == 'true'):
            childData.append(None)
        childData.append(getFirstChildData(elem))
    return childData

def isNil(elem):
    if (elem is None):
        raise ValueError('elem is None')
    if (elem.getAttribute('xsi:nil') == 'true'):
        return True
    return False

