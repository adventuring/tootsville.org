if (!('Tank' in Tootsville)) { Tootsville.Tank = { VectorTextureBuilder: {} } };
if (!('VectorTextureBuilder' in Tootsville.Tank)) { Tootsville.Tank.VectorTextureBuilder = {}; };


Tootsville.Tank.VectorTextureBuilder.build = function (svgData, name)
{ const svgXML = svgData.documentElement;
  const svgString = new XMLSerializer ().serializeToString (svgXML);
  const svgBase64 = 'data:image/svg+xml;base64,' + window.btoa (svgString);
  const svgTexture = BABYLON.Texture.LoadFromDataString ('data', svgBase64, Tootsville.Tank.scene);
  const svgMaterial = new BABYLON.PBRMaterial (name, Tootsville.Tank.scene);
  const svgMaterial.albedoTexture = svgTexture;
  return svgMaterial; }
