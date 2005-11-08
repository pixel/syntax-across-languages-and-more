#include <iostream>

// !! memory handling not done

// An Abstract Base Class (interface)

class Shape {
public:
  virtual ostream &draw(ostream& os) const = 0;
  virtual const Shape *offset_by(const float x, const float y) const = 0;
  virtual const Shape *set_dim(const float width, const float height) const = 0;
};

// This is supposed to be a list of shapes to draw
// For simplicity, we'll hold three shapes only

class Shapes {
  const Shape *s1, *s2, *s3;
public:
  Shapes(const Shape * const _s1, const Shape * const _s2, const Shape * const _s3)
    : s1(_s1), s2(_s2), s3(_s3) {}
  ostream &draw(ostream &os) { return s3->draw(s2->draw(s1->draw(os))); }
  Shapes pack(void) { return Shapes(s1->offset_by(0,1), s2->offset_by(2,3), s3->offset_by(4,5)); }
  Shapes resize(void) { return Shapes(s1->set_dim(10,10), s2->set_dim(10,11), s3->set_dim(11,12)); }
};


// Two implementations of the Abstract datatype Shape

class Rectangle: public Shape {
  float x, y;
  float width, height;

  Rectangle(const float x, const float y, const float width, const float height)
    : x(x), y(y), width(width), height(height) {}

public:
  Rectangle(const float _width, const float _height) :
    x(0), y(0), width(_width), height(_height) {}

  ostream &draw(ostream &os) const
  { return 
      os << "Drawing a rectangle [" << x << ", " << y 
         << "] - [" << x+width << ", " << y+height << "]" << endl; }

  const Rectangle *offset_by(const float x2, const float y2) const
  { return new Rectangle(x + x2, y + y2, width, height); }

  const Shape *set_dim(const float width, const float height) const
  { return Rectangle::make(x, y, width, height); }

  static const Shape * make(const float width, const float height) { return make (0,0,width,height); }
  static const Shape * make(const float x, const float y, const float width, const float height);
};

class Square: public Shape {
  float x, y;
  float size;

public:
  Square(const float x, const float y, const float size) :
    x(x), y(y), size(size) {}

  ostream &draw(ostream &os) const
  { return os << "Drawing a square [" << x << ", " << y << "] of size " << size << endl; }

  const Square *offset_by(const float x2, const float y2) const
  { return new Square(x + x2, y + y2, size); }

  const Shape *set_dim(const float width, const float height) const
  { return Rectangle::make(x, y, width, height); }
};

const Shape * Rectangle::make(const float x, const float y, const float width, const float height)
{ return width == height ? (Shape *) new Square(x,y,width) : (Shape *)new Rectangle(x,y,width,height); }


// The test

int main(void)
{
  cerr << "Instantiating shapes..." << endl;
  Shapes shapes(Rectangle::make(1,2), Rectangle::make(5,5),
		Rectangle::make(7,7));
  cerr << "Drawing ... some rectangles are actually squares" << endl;
  shapes.draw(cerr);
  cerr << "Packing ..." << endl;
  Shapes shapes_packed = shapes.pack();
  shapes_packed.draw(cerr);
  cerr << "Resizing ... rectangles turn squares and vice versa" << endl;
  Shapes shapes_resized = shapes_packed.resize();
  shapes_resized.draw(cerr);

  cerr << "\nAll done" << endl;
  return 0;
}
