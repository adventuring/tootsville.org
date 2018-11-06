if ('local' == Tootsville.cluster)
{ console.log("Local: no Rollbar"); }
else
{ ibgSdk.init({
    token: 'a1f625a30b0071644267de171fe2bed6'
}); }
