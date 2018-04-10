var Tootsville = {
    login: {},
    cluster: document.location.host.split('.').splice(1).join('.')
};

document.domain = Tootsville.cluster;
