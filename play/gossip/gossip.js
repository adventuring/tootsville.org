if (!Tootsville.gossip) { Tootsville.gossip = {}; }

Tootsville.gossip.connectedP = function() 
{ return false;};

Tootsville.gossip.sendUpdate = function(update) 
{ console.log("Gossipnet code not integrated");};

Tootsville.gossip.connectToDiscoveredPeer = function (offer) 
{ // TODO
};

if (!Tootsville.gossip.peers) 
{ Tootsville.gossip.peers = [];}

if (!Tootsville.gossip.iceServers) 
{ Tootsville.gossip.iceServers = [ { urls: [ 'stun:stun.l.google.com:19302' ] } ];}

Tootsville.gossip.setConnectionHandlers = function(connection) 
{ connection.onconnectionstatechange = event => console.log(`[gossip] onconnectionstatechange`, event);
  connection.ondatachannel = event => console.log(`[gossip] ondatachannel`);

  connection.oniceconnectionstatechange = event => 
  { console.log(`[gossip] oniceconnectionstatechange`, connection.iceConnectionState); };

  connection.onicegatheringstatechange = event => 
  { console.log(`[gossip] onicegatheringstatechange`, connection.iceGatheringState); };

  connection.onidentityresult = event => console.log(`[gossip] onidentityresult`, event);
  connection.onidpassertionerror = event => console.log(`[gossip] onidpassertionerror`, event);
  connection.onidpvalidationerror = event => console.log(`[gossip] onidpvalidationerror`, event);
  connection.onpeeridentity = event => console.log(`[gossip] onpeeridentity`, event);
  connection.onremovestream = event => console.log(`[gossip] onremovestream`, event);
  connection.onsignalingstatechange = event => 
  { console.log(`[gossip] onsignalingstatechange`, connection.signalingState);
    document.getElementById('signalingStateSpan').textContent = connection.signalingState; };
  connection.ontrack = event => console.log(`[gossip] ontrack`, event);};

Tootsville.gossip.createOffer = function () 
{ var connection = new RTCPeerConnection({ iceServers: Tootsville.gossip.iceServers });
  Tootsville.gossip.setConnectionHandlers(connection);

  connection.gossipChannel = connection.createDataChannel('test');
  connection.gossipChannel.onbufferedamountlow = event => console.log(`[gossip] onbufferedamountlow`, event);
  connection.gossipChannel.onclose = event => console.log(`[gossip] onclose`, event);
  connection.gossipChannel.onerror = event => console.log(`[gossip] onerror`, event);
  connection.gossipChannel.onopen = event => console.log(`[gossip] onopen`, event);};

Tootsville.gossip.linkOfferToPeer = function(peer) 
{ connection.onicecandidate = event => 
  { if (event.candidate) 
    { Tootsville.gossip.peerICECandidate(peer, event.candidate, event); } };
  connection.gossipChannel.onmessage = event => Tootsville.gossip.acceptMessage(peer, message);
  connection.createOffer().then(
      offer => 
          { connection.setLocalDescription(offer).then
            (() => 
             { Tootsville.util.rest('POST', 'gossip/peer-offer',
                                    { peer: peer,
                                      offer: { type: offer.type, sdp: offer.sdp } }); }); }); };

Tootsville.gossip.createOffers = function(count) 
{ var offers = [];
  for (var i = 0; i < count; ++i) 
  { offers = offers.append(Tootsville.gossip.createOffer()); }
  return offers; };

Tootsville.gossip.peerDataChannel = function(peer, event) 
{ console.log(`[gossip] ondatachannel`);
  dataChannel = event.channel;

  dataChannel.onbufferedamountlow = event => console.log(`[gossip] onbufferedamountlow`, event);
  dataChannel.onclose = event => console.log(`[gossip] onclose`, event);
  dataChannel.onerror = event => console.log(`[gossip] onerror`, event);
  dataChannel.onmessage = event => Tootsville.gossip.acceptMessage(peer, message);

  dataChannel.onopen = event => console.log(`[gossip] onopen`); }

Tootsville.gossip.peerICECandidate = function (peer, candidate, event)
{ Tootsville.util.rest('POST', 'gossip/peer-candidate',
                       { peer: peer,
                         candidate: JSON.parse(JSON.stringify(event.candidate)) }); }

Tootsville.gossip.connectToOffer = function(offer) 
{ var peerConnection = new RTCPeerConnection({ iceServers: Tootsville.gossip.iceServers });

  peerConnection.onconnectionstatechange = event => console.log(`[gossip] onconnectionstatechange`, event);

  peerConnection.ondatachannel = event => 
  { Tootsville.gossip.peerDataChannel(peer, event);

    peerConnection.onicecandidate = event => 
    { if (event.candidate) 
      { Tootsville.gossip.peerICECandidate(peer, event.candidate, event); } };

    peerConnection.oniceconnectionstatechange = event => 
    { console.log(`[gossip] oniceconnectionstatechange`, peerConnection.iceConnectionState); };

    peerConnection.onicegatheringstatechange = event => 
    { console.log(`[gossip] onicegatheringstatechange`, peerConnection.iceGatheringState); };

    peerConnection.onidentityresult = event => console.log(`[gossip] onidentityresult`, event);
    peerConnection.onidpassertionerror = event => console.log(`[gossip] onidpassertionerror`, event);
    peerConnection.onidpvalidationerror = event => console.log(`[gossip] onidpvalidationerror`, event);
    peerConnection.onpeeridentity = event => console.log(`[gossip] onpeeridentity`, event);
    peerConnection.onremovestream = event => console.log(`[gossip] onremovestream`, event);
    peerConnection.onsignalingstatechange = event => 
    { console.log(`[gossip] onsignalingstatechange`, peerConnection.signalingState); };

    peerConnection.ontrack = event => { console.log(`[gossip] ontrack`, event) };

    peerConnection.setRemoteDescription(offer).then(
        () => 
            { console.log(`[gossip] set offerer description`);
              peerConnection.createAnswer().then(
                  answer => 
                      { peerConnection.setLocalDescription(answer).then
                        ( () => 
                          { Tootsville.util.rest('POST', 'gossip/peer-answer',
                                                 { name: name,
                                                   peer: peer,
                                                   answer: { type: answer.type, sdp: answer.sdp } }); }); }); }); }
}


Tootsville.gossip.connect = function() 
{ var offers = Tootsville.gossip.createOffers(5);
  Tootsville.util.rest('GET', 'gossip/directory/', { offers: offers }).then(
      response => 
          { Tootsville.inform("Gossip", "Got directory", response.offers.length + ' offers');
            response.offers.forEach(Tootsville.gossip.connectToOffer); });
};

Tootsville.gossip.ensureConnected = function() 
{ return new Promise( (success, failure) => 
                      { if (Tootsville.gossip.connectedP()) 
                        { success(); } else 
                        { Tootsville.gossip.connect().then(success, failure); } });
};
