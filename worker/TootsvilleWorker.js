if (!('TootsvilleWorker' in window))
{ window.TootsvilleWorker = {} };
TootsvilleWorker.cluster = document.location.host.split('.').splice(1).join('.');
