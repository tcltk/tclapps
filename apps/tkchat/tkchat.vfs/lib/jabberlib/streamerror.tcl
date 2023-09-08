#  streamerror.tcl --
#  
#      This file is part of the jabberlib. It provides english clear text
#      messages that gives some detail of 'urn:ietf:params:xml:ns:xmpp-streams'.
#      
#  Copyright (c) 2004  Mats Bengtsson
#  
# $Id: streamerror.tcl,v 1.2 2004/09/18 14:43:29 matben Exp $
# 

# The syntax for stream errors is as follows:
#
#   <stream:error>
#      <defined-condition xmlns='urn:ietf:params:xml:ns:xmpp-streams'/>
#      <text xmlns='urn:ietf:params:xml:ns:xmpp-streams'>
#        OPTIONAL descriptive text
#      </text>
#      [OPTIONAL application-specific condition element]
#   </stream:error>

package provide streamerror 1.0

namespace eval streamerror {
    
    # This maps Defined Conditions to clear text messages.
    # draft-ietf-xmpp-core23; 4.7.3 Defined Conditions
    
    variable msg
    array set msg {
	bad-format            {the entity has sent XML that cannot be processed.}
	bad-namespace-prefix  {the entity has sent a namespace prefix\
	   that is unsupported, or has sent no namespace prefix on an element\
	   that requires such a prefix.}
	conflict              {the server is closing the active stream for this\
	   entity because a new stream has been initiated that conflicts with\
	   the existing stream.}
	connection-timeout    {the entity has not generated any traffic\
	   over the stream for some period of time.}
	host-gone             {the value of the 'to' attribute provided by the\
	   initiating entity in the stream header corresponds to a hostname\
	   that is no longer hosted by the server.}
	host-unknown          {the value of the 'to' attribute provided by the\
	   initiating entity in the stream header does not correspond to a\
	   hostname that is hosted by the server.}
	improper-addressing   {a stanza sent between two servers lacks\
	   a 'to' or 'from' attribute.}
	internal-server-error {the server has experienced a\
	   misconfiguration or an otherwise-undefined internal error that\
	   prevents it from servicing the stream.}
	invalid-from          {the JID or hostname provided in a 'from'\
	   address does not match an authorized JID or validated domain\
	   negotiated between servers via SASL or dialback, or between a\
	   client and a server via authentication and resource binding.}
	invalid-id            {the stream ID or dialback ID is invalid or does\
	   not match an ID previously provided.}
	invalid-namespace     {the streams namespace name is something\
	   other than "http://etherx.jabber.org/streams" or the dialback\
	   namespace name is something other than "jabber:server:dialback".}
	invalid-xml           {the entity has sent invalid XML over the stream\
	   to a server that performs validation.}
	not-authorized        {the entity has attempted to send data before\
	   the stream has been authenticated, or otherwise is not authorized\
	   to perform an action related to stream negotiation; the receiving\
	   entity MUST NOT process the offending stanza before sending the\
	   stream error.}
	policy-violation      {the entity has violated some local service\
	   policy; the server MAY choose to specify the policy in the <text/>\
	   element or an application-specific condition element.}
	remote-connection-failed {the server is unable to properly\
	   connect to a remote entity that is required for authentication or\
	   authorization.}
	resource-constraint   {the server lacks the system resources\
	   necessary to service the stream.}
	restricted-xml        {the entity has attempted to send restricted\
	   XML features such as a comment, processing instruction, DTD,\
	   entity reference, or unescaped character.}
	see-other-host        {the server will not provide service to the\
	   initiating entity but is redirecting traffic to another host; the\
	   server SHOULD specify the alternate hostname or IP address (which\
	   MUST be a valid domain identifier) as the XML character data of\
	   the <see-other-host/> element.}
	system-shutdown       {the server is being shut down and all active\
	   streams are being closed.}
	undefined-condition {the error condition is not one of those\
	   defined by the other conditions in this list; this error condition\
	   SHOULD be used only in conjunction with an application-specific\
	   condition.}
	unsupported-encoding  {the initiating entity has encoded the\
	   stream in an encoding that is not supported by the server.}
	unsupported-stanza-type {the initiating entity has sent a\
	   first-level child of the stream that is not supported by the\
	   server.}
	unsupported-version   {the value of the 'version' attribute\
	   provided by the initiating entity in the stream header specifies a\
	   version of XMPP that is not supported by the server.}
	xml-not-well-formed   {the initiating entity has sent XML that\
	   is not well-formed as defined by [XML].}
    }
}

# streamerror::getmsg --
# 
#       Return the english clear text message from a defined-condition.

proc streamerror::getmsg {condition} {
    variable msg

    if {[info exists msg($condition)]} {
	return $msg($condition)
    } else {
	return ""
    }
}

#-------------------------------------------------------------------------------

