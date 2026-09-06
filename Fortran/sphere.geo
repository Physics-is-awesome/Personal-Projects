// Gmsh script to generate a tetrahedral mesh of a 3D ball
// Usage: gmsh sphere.geo -format msh2 -o sphere.msh

// Parameters
R = 1.0;           // Radius of sphere
lc = 0.2;          // Characteristic mesh length (smaller = finer mesh)

// Points on the surface
Point(1) = {0, 0, 0, lc};       // center
Point(2) = {R, 0, 0, lc};
Point(3) = {-R, 0, 0, lc};
Point(4) = {0, R, 0, lc};
Point(5) = {0, -R, 0, lc};
Point(6) = {0, 0, R, lc};
Point(7) = {0, 0, -R, lc};

// Intermediate points for octants
Point(8) = {R/Sqrt(2), R/Sqrt(2), 0, lc};
Point(9) = {-R/Sqrt(2), R/Sqrt(2), 0, lc};
Point(10) = {-R/Sqrt(2), -R/Sqrt(2), 0, lc};
Point(11) = {R/Sqrt(2), -R/Sqrt(2), 0, lc};

Point(12) = {R/Sqrt(2), 0, R/Sqrt(2), lc};
Point(13) = {-R/Sqrt(2), 0, R/Sqrt(2), lc};
Point(14) = {-R/Sqrt(2), 0, -R/Sqrt(2), lc};
Point(15) = {R/Sqrt(2), 0, -R/Sqrt(2), lc};

Point(16) = {0, R/Sqrt(2), R/Sqrt(2), lc};
Point(17) = {0, -R/Sqrt(2), R/Sqrt(2), lc};
Point(18) = {0, -R/Sqrt(2), -R/Sqrt(2), lc};
Point(19) = {0, R/Sqrt(2), -R/Sqrt(2), lc};

// Circles on the sphere surface
Circle(1) = {2, 1, 4};          // equator 1
Circle(2) = {4, 1, 3};          // equator 2
Circle(3) = {3, 1, 5};          // equator 3
Circle(4) = {5, 1, 2};          // equator 4

Circle(5) = {2, 1, 6};          // meridian 1
Circle(6) = {6, 1, 3};          // meridian 2
Circle(7) = {3, 1, 7};          // meridian 3
Circle(8) = {7, 1, 2};          // meridian 4

Circle(9) = {4, 1, 6};          // meridian 5
Circle(10) = {6, 1, 5};         // meridian 6
Circle(11) = {5, 1, 7};         // meridian 7
Circle(12) = {7, 1, 4};         // meridian 8

// Curve loops forming the 8 octants
Curve Loop(1) = {1, 9, -5};
Surface(1) = {1};

Curve Loop(2) = {2, -6, -9};
Surface(2) = {2};

Curve Loop(3) = {3, -10, -6};
Surface(3) = {3};

Curve Loop(4) = {4, 5, -10};
Surface(4) = {4};

Curve Loop(5) = {1, -12, 8};
Surface(5) = {5};

Curve Loop(6) = {2, 7, -12};
Surface(6) = {6};

Curve Loop(7) = {3, 11, -7};
Surface(7) = {7};

Curve Loop(8) = {4, -11, 8};
Surface(8) = {8};

// Surface loop for volume
Surface Loop(1) = {1, 2, 3, 4, 5, 6, 7, 8};
Volume(1) = {1};

// 3D mesh (tetrahedral)
// Use Delaunay algorithm for robust 3D meshing
Mesh.Algorithm = 6;            // Delaunay in 3D
Mesh.Algorithm3D = 4;          // Frontal-Delaunay

// Optional: Add refinement near the surface
Field[1] = Distance;
Field[1].Sampling = 100;
Field[2] = Threshold;
Field[2].InField = 1;
Field[2].SizeMin = lc / 2;
Field[2].SizeMax = lc;
Field[2].DistMin = 0.1 * R;
Field[2].DistMax = 0.5 * R;
Background Field = 2;

// Generate mesh
// Mesh.CharacteristicLengthMin = lc / 4;
// Mesh.CharacteristicLengthMax = lc;
