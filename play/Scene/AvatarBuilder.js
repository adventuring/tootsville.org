/* -*- js2 -*-*/

/**@license
 *
 * play/Scene/AvatarBuilder.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018-2020   The
 * Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software:  you can redistribute it and/or modify
 * it  under the  terms  of the  GNU Affero  General  Public License  as
 * published by  the Free Software  Foundation; either version 3  of the
 * License, or (at your option) any later version.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.  See the  GNU
 * Affero General Public License for more details.
 *
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 *
 */

if (!('AvatarBuilder' in Tootsville)) { Tootsville.AvatarBuilder = { }; }

/**
*
*/
Tootsville.AvatarBuilder.patterns = {
    lightning: 'm 165.49363,109.39907 49.02812,-11.852952 3.23262,56.032142 -14.00803,1.07754 12.93049,34.48131 -12.93049,3.7714 5.92648,21.55082 -26.39976,-33.94254 8.08156,-1.07755 -22.62836,-31.78746 17.77942,-4.31016 z',
    flowers: 'm 60.768457,15.872906 a 17.906584,17.906584 0 0 0 -15.49711,8.97991 17.906583,17.906583 0 0 0 -3.0334,17.27884 17.906583,17.906583 0 0 0 0.0641,0.15333 17.906584,17.906584 0 0 0 -2.50572,-0.19673 17.906584,17.906584 0 0 0 -11.293695,4.0266 17.906583,17.906583 0 0 0 -8.651763,21.84227 17.906583,17.906583 0 0 0 22.540388,11.01691 17.906584,17.906584 0 0 0 -3.60726,10.73177 17.906584,17.906584 0 0 0 6.93216,14.138724 17.906583,17.906583 0 0 0 18.31824,3.94855 17.906583,17.906583 0 0 0 11.91295,-16.086914 17.906583,17.906583 0 0 0 20.529265,6.48096 17.906583,17.906583 0 0 0 11.577238,-13.38883 17.906584,17.906584 0 0 0 0.91476,-5.57855 17.906584,17.906584 0 0 0 -10.039238,-16.06221 17.906583,17.906583 0 0 0 4.813928,-3.5738 17.906584,17.906584 0 0 0 6.97318,-14.15305 17.906584,17.906584 0 0 0 -17.906458,-17.90647 17.906584,17.906584 0 0 0 -12.895805,5.51801 17.906583,17.906583 0 0 0 -1.25485,1.05813 17.906584,17.906584 0 0 0 0.016,-0.32098 17.906584,17.906584 0 0 0 -17.90686,-17.90647 z m 6.9911,31.3395 a 13.947631,13.947631 0 0 1 13.94756,13.94756 13.947631,13.947631 0 0 1 -13.94756,13.94756 13.947631,13.947631 0 0 1 -13.94756,-13.94756 13.947631,13.947631 0 0 1 13.94756,-13.94756 z',
    hearts: 'm 100.25258,66.538316 q 11.47502,0 18.03218,13.283896 2.37414,5.257031 2.4872,8.026865 h 0.16958 q 1.97845,-9.157409 7.17895,-15.205821 6.10494,-6.10494 13.73612,-6.10494 11.81419,0 18.93661,12.662097 1.80888,4.691759 1.80888,8.818245 0,14.131802 -11.75767,26.963482 l -29.90289,35.89478 h -0.33917 L 88.834079,111.92967 q -9.496573,-11.58808 -9.496573,-23.911012 0,-11.927242 11.248916,-18.936617 4.748286,-2.543725 9.666158,-2.543725 z',
    spots: 'm 57.215385,196.89556 c 14.16871,-24.21061 1.717258,-38.50789 17.542087,-57.92546 15.824829,-19.41758 45.663718,-28.29696 69.769138,-23.74938 24.10542,4.54759 76.65345,-29.70492 82.03089,22.08967 5.37742,51.79459 -54.464,54.58715 -71.3929,70.80937 -16.9289,16.22223 -30.27675,66.61784 -59.105076,70.14046 -28.828314,3.52262 -66.252802,-45.7833 -52.566003,-65.1775 13.6868,-19.3942 13.721864,-16.18716 13.721864,-16.18716 z',
    stars: 'M 120.57059,161.66458 84.512458,153.18534 55.146457,175.76241 52.068123,138.84886 21.521458,117.89683 55.677073,103.56225 66.164197,68.036112 90.351861,96.090402 127.37992,95.086069 108.17311,126.75916 Z',
    'polka dots': 'M 215.44643,104.23215 A 49.892857,49.892857 0 0 1 165.55357,154.125 49.892857,49.892857 0 0 1 115.66072,104.23215 49.892857,49.892857 0 0 1 165.55357,54.339291 49.892857,49.892857 0 0 1 215.44643,104.23215',
    sparkles: 'm 51.782021,257.31179 3.046568,19.87637 -10.010816,-17.43948 -13.640473,14.77462 7.393094,-18.70011 -20.05596,-1.45272 19.229853,-5.87918 -11.368899,-16.58614 16.58614,11.3689 5.879174,-19.22985 1.452726,20.05596 18.700109,-7.39309 -14.774621,13.64047 17.439482,10.01081 z',
    hearts: 'm 100.25258,66.538316 q 11.47502,0 18.03218,13.283896 2.37414,5.257031 2.4872,8.026865 h 0.16958 q 1.97845,-9.157409 7.17895,-15.205821 6.10494,-6.10494 13.73612,-6.10494 11.81419,0 18.93661,12.662097 1.80888,4.691759 1.80888,8.818245 0,14.131802 -11.75767,26.963482 l -29.90289,35.89478 h -0.33917 L 88.834079,111.92967 q -9.496573,-11.58808 -9.496573,-23.911012 0,-11.927242 11.248916,-18.936617 4.748286,-2.543725 9.666158,-2.543725 z',
    notes: 'm 80.606971,40.864342 h 6.59855 l 0.606763,0.530917 v 2.957971 q 2.047826,4.095652 15.169086,8.873913 18.20289,5.309178 18.20289,15.775844 0,11.073429 -9.25314,18.354588 h -0.30338 l -2.27536,-2.427053 q 4.62657,-3.716425 4.62657,-10.087439 0,-5.309178 -17.292753,-10.694202 -6.977777,-3.185507 -8.873913,-5.157487 V 139.8426 q 0,11.60434 -16.98937,15.01739 h -4.323189 q -18.354588,0 -18.354588,-12.74203 0,-9.93575 16.079226,-11.68019 h 1.744445 q 7.281159,0 14.0314,4.8541 V 41.395259 Z',
    swirls: 'm 87.369141,76.646484 c 1.481042,0.07955 2.978204,0.242469 4.486328,0.494141 18.902771,3.154454 34.308621,21.149688 29.933591,41.416015 -1.60778,7.44768 -5.91952,13.88884 -12.01367,18.23242 -6.09415,4.34359 -14.512243,6.45022 -22.738281,3.89453 -5.493571,-1.70675 -9.939358,-5.21482 -12.921875,-10.20703 -2.982516,-4.99221 -4.031718,-12.53329 -0.447265,-18.80078 2.680378,-4.68669 5.405412,-6.12556 9.796875,-7.35937 2.195731,-0.61691 5.163867,-1.05243 8.607422,0.43554 3.443554,1.48797 6.135174,5.33042 6.83789,8.55078 a 9.1509146,9.1509146 0 0 1 -4.408203,10.03711 c 1.568684,0.0203 3.078142,-0.33111 4.652344,-1.45312 2.244843,-1.60001 4.160723,-4.47074 4.748043,-7.19141 1.95198,-9.04207 -5.706318,-17.943217 -15.05859,-19.503904 -13.145937,-2.193765 -25.439279,8.623844 -27.255859,21.888674 -1.92975,14.0912 7.404716,28.04148 20.927734,32.98828 a 9.1509146,9.1509146 0 1 1 -6.287109,17.18555 C 54.934115,159.46428 40.27149,137.85876 43.457031,114.59766 46.398161,93.121273 65.153508,75.453216 87.369141,76.646484 Z',
    horseshoes: 'm 759.00977,394.97461 c -1.29532,0.007 -2.54367,0.11528 -3.75,0.30859 -15.23288,-0.95128 -29.30482,0.75397 -38.05079,5.12696 -16.16243,8.08119 -30.30508,30.30397 -34.3457,47.47656 -4.04062,17.17262 -27.27344,96.97461 -27.27344,96.97461 l -12.12304,-3.03125 -10.10157,48.48828 63.64063,15.15234 c 0,0 20.20165,-132.32975 44.44531,-148.49218 24.24367,-16.16244 37.37695,2.01953 37.37695,2.01953 l 0.88868,-0.11914 c 1.8142,2.04842 3.59743,4.49781 5.30859,7.45312 14.60001,25.21546 -25.78125,152.8418 -25.78125,152.8418 l 63.7793,14.54492 12.37695,-47.95703 -12.21289,-2.64258 c 0,0 14.44226,-81.84951 18.41016,-99.03906 3.9679,-17.18956 1.1079,-43.37547 -9.81641,-57.76953 -10.92431,-14.39418 -42.17745,-28.6737 -70.20117,-31.22461 -0.87574,-0.0797 -1.73266,-0.11611 -2.57031,-0.11133 z',
    patches: 'm -34.884705,123.87823 h 98.273804 v 99.78571 h -98.273804 z',
    blank: '',
    moons: 'm 293.9498,562.16534 q 2.70019,0 4.55957,2.57715 0.72461,1.18262 0.72461,2.53614 0,2.67968 -2.70703,4.43652 -1.22364,0.66992 -2.42676,0.66992 h -0.33496 q -0.32129,0 -0.875,-0.10254 v -0.041 q 1.89355,-0.54004 2.74805,-1.75 1.05957,-1.38086 1.05957,-3.21289 0,-2.73438 -2.28321,-4.26563 -0.92969,-0.54687 -1.56543,-0.65625 v -0.0205 q 0,-0.0752 1.01856,-0.1709 z m 0.1914,0.42383 v 0.0205 q 3.12403,1.34668 3.12403,4.70996 0,2.74805 -2.30371,4.33399 l -0.29395,0.16406 v 0.0273 h 0.041 q 0.58789,0 1.66797,-0.56738 1.59277,-1.03906 2.15332,-2.70703 0.19824,-0.63574 0.19824,-1.27149 v -0.0615 q 0,-2.16699 -2.37207,-3.95117 -1.11426,-0.69727 -2.19434,-0.69727 z',
    suns: 'M 138.76865,102.46302 104.21426,86.422568 125.08688,118.29161 97.613068,91.900481 107.00689,128.8201 89.590871,94.938181 86.487928,132.90756 81.016998,95.206487 65.753544,130.11106 72.820563,92.676323 47.050628,120.73363 65.889776,87.621873 32.405934,105.79148 60.975695,80.590864 23.40644,86.903803 58.610838,72.345215 21.02738,66.117384 59.051474,63.77847 25.526565,45.684751 62.249852,55.818969 36.416436,27.820097 67.859379,49.329247 52.516907,14.459335 75.272175,45.012567 72.083242,7.0503105 83.684949,43.336707 92.995126,6.395906 92.186047,44.483274 112.98643,12.567037 99.854242,48.328018 129.89079,24.894965 l -24.03222,29.559337 36.01778,-12.410534 -32.32799,20.154481 38.09594,-0.04314 -37.12053,8.565574 36.04581,12.328925 -37.89047,-3.951546 z',
    props: 'M 138.76865,102.46302 C 123.05871,128.23492 114.50749,77.090625 84.333398,76.371366 54.15931,75.652108 43.180929,126.33088 28.716782,99.839721 14.252635,73.348565 62.820508,91.515133 78.530448,65.743235 94.240388,39.971338 55.840476,5.1243967 86.014564,5.843656 116.18865,6.5629154 76.172006,39.540643 90.636154,66.031799 105.1003,92.522955 154.47859,76.691126 138.76865,102.46302 Z',
    snowflake: 'M 322.36523 22.054688 C 261.78277 22.086563 294.82545 74.30468 322.48828 131.44336 C 353.28779 74.869017 389.37665 23.618247 325.09375 22.085938 C 324.16379 22.06377 323.25452 22.05422 322.36523 22.054688 z M 322.48828 131.44336 C 317.33418 140.91072 312.3286 150.51609 307.9668 160.07422 C 278.1284 129.76113 212.68465 46.795261 204.25391 51.398438 C 195.82311 56.001643 230.23342 155.91401 239.59961 197.4043 C 229.20033 195.90543 218.41344 194.91997 207.66211 194.13672 C 213.28382 203.33366 219.10083 212.47108 225.19727 221.02734 C 184.02608 231.71161 79.453521 246.905 79.224609 256.50781 C 78.995708 266.11058 182.72604 286.26657 223.3418 298.90039 C 216.84439 307.15663 210.6006 316.00688 204.54688 324.92578 C 215.32222 324.65569 226.14177 324.18708 236.59961 323.18555 C 225.26702 364.18271 186.13798 462.34214 194.33984 467.3418 C 202.54165 472.34142 271.86428 392.5877 303.11328 363.73047 C 307.01452 373.48496 311.55509 383.31795 316.25195 393.01953 C 321.40601 383.55224 326.41167 373.94672 330.77344 364.38867 C 360.61184 394.70177 426.05558 477.6696 434.48633 473.06641 C 442.91697 468.46329 408.50744 368.55201 399.14062 327.06055 C 409.53922 328.55931 420.32555 329.54295 431.07617 330.32617 C 425.4544 321.12916 419.63752 311.99181 413.54102 303.43555 C 454.71091 292.75126 559.2868 277.55804 559.51562 267.95508 C 559.7445 258.35231 456.0142 238.19632 415.39844 225.5625 C 421.89551 217.30665 428.13992 208.45752 434.19336 199.53906 C 423.41802 199.80916 412.59846 200.27581 402.14062 201.27734 C 413.47337 160.27994 452.60223 62.120737 444.40039 57.121094 C 436.19856 52.121455 366.87585 131.87731 335.62695 160.73438 C 331.72551 150.97931 327.18543 141.14551 322.48828 131.44336 z M 434.19336 199.53906 C 498.58835 197.92495 561.0197 203.55432 530.20508 147.11719 C 499.39044 90.68002 470.3693 146.24153 434.19336 199.53906 z M 431.07617 330.32617 C 464.6714 385.28668 491.01187 442.1664 524.48047 387.26172 C 557.94911 332.35697 495.32104 335.00665 431.07617 330.32617 z M 316.25195 393.01953 C 285.45242 449.59397 249.36349 500.84465 313.64648 502.37695 C 377.92966 503.90926 344.32096 450.99739 316.25195 393.01953 z M 204.54688 324.92578 C 140.1519 326.53989 77.720538 320.91053 108.53516 377.34766 C 139.34962 433.78448 168.37124 378.22302 204.54688 324.92578 z M 207.66211 194.13672 C 174.06724 139.1766 147.7281 82.296921 114.25977 137.20117 C 80.791461 192.10537 143.41792 189.45644 207.66211 194.13672 z ',
    x: 'm 75.82007,51.181786 c -1.097346,2.291661 -5.528292,1.861977 -4.933283,0.444169 -3.111425,3.693786 -10.023277,2.664262 -11.5606,7.974786 -1.802278,5.551651 -6.556406,9.258142 -9.832731,13.846325 -2.055918,3.346437 -1.993116,9.246416 2.491441,10.479826 5.801509,0.924632 11.659213,1.7781 17.247884,3.669774 5.210156,-1.09164 7.639769,7.305563 1.508437,7.260635 -7.090087,0.987811 -13.564858,4.192504 -20.449394,5.987889 -2.67834,0.74567 -6.231935,1.48608 -3.930482,4.86128 1.030481,2.9004 -3.081598,1.07849 -0.22497,3.09632 2.683216,3.4479 2.617078,9.36936 7.514932,10.76834 4.999973,1.78553 8.744268,5.69943 12.626166,9.11073 3.769042,2.93397 10.991838,2.42383 11.471429,-3.28966 1.026291,-4.68493 1.068425,-9.58014 2.703355,-14.12221 -0.08727,-3.38749 3.956968,-10.54427 7.057383,-5.70097 1.468581,4.72843 2.232181,9.66179 4.296967,14.19552 1.459162,4.49982 2.407621,9.18523 4.423916,13.48922 1.657824,0.24723 4.20624,-2.79306 5.01608,-0.0542 3.63974,-3.2189 10.28203,-2.94596 11.86923,-8.3292 1.65173,-5.79788 7.22432,-9.07784 9.95557,-14.18784 1.76274,-3.42328 0.97902,-9.0382 -3.46826,-9.71901 -5.64185,-0.7841 -11.29052,-1.74353 -16.71025,-3.5407 -5.208668,1.091707 -7.641114,-7.303787 -1.51031,-7.260092 7.09047,-0.98815 13.56575,-4.192626 20.45071,-5.988158 2.67773,-0.745768 6.2328,-1.485924 3.93123,-4.861095 -1.03095,-2.900161 3.08108,-1.079465 0.22458,-3.097025 -2.68308,-3.448065 -2.61698,-9.369818 -7.51519,-10.768438 -5.00007,-1.785158 -8.74379,-5.699676 -12.62598,-9.110491 -3.76961,-2.934794 -10.986,-2.420146 -11.470448,3.289726 -1.030729,4.684184 -1.066681,9.580779 -2.703966,14.122404 0.0877,3.387873 -3.958112,10.544434 -7.057752,5.700139 -1.45119,-4.760384 -2.289318,-9.694767 -4.286874,-14.277659 -1.48074,-4.670225 -2.735125,-9.417554 -4.50882,-13.990335 z m 10.218757,41.230787 c 0.462367,0.214693 -0.196391,0.248065 0,0 z',
    picks: 'm 44.477948,41.75522 c 0,0 -15.689711,57.265537 3.680905,67.14637 C 73.294665,121.72321 109.95207,94.178601 108.80965,73.050671 107.57056,50.13493 44.477948,41.75522 44.477948,41.75522 Z'
};

/**
 *
 */
Tootsville.AvatarBuilder.getPathForPattern = function (pattern) {
    /* FIXME:  spots  are  irregular,   as  are  patches  and  sparkles.
     * They need to randomly choose from among several paths. */
    return new Path2D (Tootsville.AvatarBuilder.patterns [ pattern.toLowerCase () ] ||
                       Tootsville.AvatarBuilder.patterns.spots ); };

/**
* Pick a random color that does not match the avatar's skin
*/
Tootsville.AvatarBuilder.rainbowColor = function (baseColor)
{ const patternColors = [ "Black", "Cyan", "Indigo", "Orange", "Pink", "Turquoise", "Violet", "White", "Yellow" ];
  let color = patternColors [ Math.floor (Math.random () * patternColors.length) ];
  if (color == baseColor)
  { return Tootsville.AvatarBuilder.rainbowColor (baseColor); }
  else
  { return color; } };

Tootsville.AvatarBuilder.drawPatternOnCanvas = function (avatar, canvas)
{ canvas.fillStyle = interpretTootColor (avatar.baseColor);;
  canvas.fillRect (0, 0, 1024, 1024);
  for (let x = 0; x < 1023; x += 192)
  { for (let y = 0; y < 1023; y += 192)
    { canvas.setTransform (1, 0, 0, 1, x, y);
      if (avatar.patternColor == 'rainbow')
      { canvas.fillStyle = Tootsville.AvatarBuilder.rainbowColor (avatar.baseColor); }
      else
      { canvas.fillStyle = interpretTootColor (avatar.patternColor); }
      canvas.fill (Tootsville.AvatarBuilder.getPathForPattern (avatar.pattern)); } } };

/**
 * Colorize an Avatar and apply their pattern
 */
Tootsville.AvatarBuilder.colorize = function (avatar, model, scene, finish)
{ if (! model) { return; }
  const patternTexture = new BABYLON.DynamicTexture (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     1024 /* resolution */,
                                                     scene);
  const canvas = patternTexture.getContext ();
  Tootsville.AvatarBuilder.drawPatternOnCanvas (avatar, canvas);
  patternTexture.update ();
  const skinMaterial = new BABYLON.StandardMaterial (avatar.baseColor + "/" + avatar.pattern + "/" + avatar.patternColor,
                                                     scene);
  skinMaterial.diffuseTexture = patternTexture;
  
  const padMaterial = new BABYLON.StandardMaterial (avatar.padColor,
                                                    scene);
  padMaterial.diffuseColor = new BABYLON.Color3.FromHexString (interpretTootColor (avatar.padColor));
  const eyeMaterial = new BABYLON.StandardMaterial ("eye", scene);
  eyeMaterial.diffuseColor = new BABYLON.Color3.FromHexString ('#000000');
  const meshes = model.getChildMeshes ();
  for (let i = 0; i < meshes.length; ++i)
  { const mesh = meshes[i];
    mesh.material = (
        mesh.name.indexOf ('Eye') >= 0  ? eyeMaterial :
            mesh.name.indexOf ('Skin') >= 0 ? skinMaterial :
            mesh.name.indexOf ('Pad') >= 0 ? padMaterial :
            eyeMaterial ); }
  // console.debug ("Colorized " + meshes.length + " meshes for avatar "+ avatar.avatar + ' ' + avatar.userName);
  if (finish) { finish (model); } };

/**
 * Adds a nametag to an avatar. (Only in the main scene, for now.)
 */
Tootsville.AvatarBuilder.addNameTag = function (avatar, model, scene)
{ if (scene !== Tootsville.Tank.scene) { return; }
  var nameTag = document.createElement ('DIV');
  nameTag.innerHTML = avatar.userName; /* Note this may start with ◆ */
  nameTag.tootName = avatar.name;
  nameTag.className = 'name-tag';
  nameTag.addEventListener ('click', Tootsville.UI.HUD.nameTagClicked);
  document.getElementById('hud').append (nameTag);
  Tootsville.Tank.findAvatar (avatar.name).nameTag = nameTag;
  Tootsville.UI.HUD.refreshNameTagAttachment (model, nameTag); };


/**
 * Enable the object to cast shadows in the scene
 */
Tootsville.AvatarBuilder.enableShadows = function (object, scene)
{ if (Tootsville.Tank.shadowGenerator)
  { /* Tootsville.Tank.shadowGenerator.addShadowCaster (object); */
      Tootsville.Tank.shadowGenerator.getShadowMap ().renderList.push (
          Tootsville.Tank.getLargestChildMesh (object)); }; };

/**
 * Add the avatar to the global list of avatars in the scene
 */
Tootsville.AvatarBuilder.rememberAvatar = function (avatar, object, scene)
{ if (Tootsville.Tank.avatars [ avatar.name ].model)
  { console.error ("Already remembered a model for avatar " + avatar.name);
    if (Tootsville.Tank.avatars [ avatar.name ].model != object)
    { object.dispose (); }
    return; }
  Tootsville.Tank.avatars [ avatar.name ] = Object.assign ((Tootsville.Tank.avatars [avatar.name] || {}), avatar);
  console.log ("Remembering model ", object, " for avatar ", avatar.name);
  Tootsville.Tank.avatars [avatar.name].model = object; };

/**
 * Actually build the avatar.
 *
 * Don't call this directly, call `Tootsville.AvatarBuilder.build'.
 */
Tootsville.AvatarBuilder.postBuild = function (avatar, model, scene)
{ console.debug ("Building " + avatar.avatar + " " + avatar.userName);
  let existing = Tootsville.Tank.findAvatar (avatar.name);
  if (existing.model)
  { console.warn ("Canceling AvatarBuilder.postBuild for " + avatar.userName + " because found one existing");
    return; }
  // TODO set scaling
  try {Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  let a = Tootsville.Tank.avatars [avatar.name];
  if (!a) { return; }
  try { Tootsville.AvatarBuilder.addNameTag (a, model, scene); } catch (e) { console.error (e); }
  try { Tootsville.AvatarBuilder.enableShadows (model, scene); } catch (e) { console.error (e); }
  try { Tootsville.Game.GravitySystem.register (a, model); } catch (e) { console.error (e); }
  if (avatar.npc)
  { try { Tootsville.Game.NPCSystem.register (a); } catch (e) { console.error (e); } } };

/**
 * Load the base avatar model from Jumbo.
 */
Tootsville.AvatarBuilder.loadAvatarBase = function (avatar, scene, finish)
{ var assetsManager = scene.assetManager;
  if (! assetsManager)
  { assetsManager = scene.assetsManager = new BABYLON.AssetsManager (scene); }
  assetsManager.useDefaultLoadingScreen = false;
  if (scene == Tootsville.Tank.scene)
  { if (Tootsville.Tank.avatars [ avatar.name ] && Tootsville.Tank.avatars [ avatar.name ].model)
    { console.warn ("Almost re-loaded avatar model for " + avatar.name);
      return; /* XXX finish? */ } }
  var loadTask = assetsManager.addMeshTask ("loading " + avatar.avatar, null,
                                            "https://jumbo.tootsville.org/Assets/Avatars/5/",
                                            avatar.avatar + ".babylon");
  loadTask.onSuccess = function (task)
  { const modelRoot = new BABYLON.Mesh ("avatar/" + avatar.name, scene);
    modelRoot.position = BABYLON.Vector3.Zero (); /* TODO */
    let i;
    for (i = 0; i < task.loadedMeshes.length; ++i)
    { modelRoot.addChild (task.loadedMeshes [i]);
      task.loadedMeshes [i].renderOutline = true;
      task.loadedMeshes [i].outlineColor = BABYLON.Color3.Black (); }
    for (i = 0; i < task.loadedParticleSystems.length; ++i)
    { modelRoot.addChild (task.loadedParticleSystems [i]); }
    for (i = 0; i < task.loadedSkeletons.length; ++i)
    { modelRoot.addChild (task.loadedSkeletons [i]); }
    console.debug ("Loaded base avatar " + avatar.avatar + " with " +
                   task.loadedMeshes.length + " meshes, " +
                   task.loadedParticleSystems.length + " particle systems,  and " +
                   task.loadedSkeletons.length + " skeletons.");
    if (scene == Tootsville.Tank.scene)
    { Tootsville.AvatarBuilder.postBuild (avatar, modelRoot, scene); }
    Tootsville.AvatarBuilder.colorize (avatar, modelRoot, scene, finish); };
  assetsManager.load (); };

/**
 *
 */
Tootsville.AvatarBuilder.update = function (avatar, model, scene, finish)
{ console.log ("Update avatar " + avatar.name + " as ", avatar, " model ", model);
  try { Tootsville.AvatarBuilder.colorize (avatar, model, scene, finish); } catch (e) { console.error (e); }
  try {Tootsville.AvatarBuilder.rememberAvatar (avatar, model, scene); } catch (e) { console.error (e); }
  if (finish) { finish (model); } };

/**
 * Build an avatar based upon the description passed in.
 *
 * The  structure  of   the  avatar  description  is   as  explained  at
 * `TOOT-INFO'.
 *
 * A duplicate of an existing avatar will not be created, but it may be updated.
 */
Tootsville.AvatarBuilder.build = function (avatar, scene, finish)
{ if (!scene) { scene = Tootsville.Tank.scene; }
  if (scene == Tootsville.Tank.scene)
  { if (Tootsville.Tank.avatars &&
        Tootsville.Tank.avatars [avatar.name] &&
        Tootsville.Tank.avatars [avatar.name].model)
    { Tootsville.AvatarBuilder.update (avatar, Tootsville.Tank.avatars [avatar.name].model, scene, finish);
      return; }
    let existing = Tootsville.Tank.findAvatar (avatar.name);
    if (existing && existing.model)
    { console.warn ("Found forgotten avatar model for " + avatar.name);
      Tootsville.Tank.avatars [ avatar.name ].model = existing.model;
      Tootsville.AvatarBuilder.update (avatar, Tootsville.Tank.avatars [avatar.name].model, scene, finish);
      return; } }
  Tootsville.AvatarBuilder.loadAvatarBase (avatar, scene, finish); };
