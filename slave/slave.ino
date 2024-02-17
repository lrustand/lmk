
#include <Wire.h>

byte keystates[4] = {0x00, 0x00, 0x00, 0x00};
byte rows[4] = {5, 6, 7, 8};
byte cols[7] = {28, 27, 26, 22, 20, 23, 21};

void setup()
{
  for (int r=0; r<4; r++)
  {
    pinMode(rows[r], OUTPUT);
  }
  for (int c=0; c<7; c++)
  {
    pinMode(cols[c], INPUT_PULLUP);
  }
  // Change I2C pins
  Wire1.setSDA(2);
  Wire1.setSCL(3);
  // Start listening on address 0x08
  Wire1.begin(0x08);
  // Register callback
  Wire1.onRequest(requestEvent);
}

void loop()
{
  for (int r=0; r<4; r++)
  {
    digitalWrite(rows[r], LOW);
    for (int c=0; c<7; c++)
    {
      bool key = !digitalRead(cols[c]);
      if (key)
      {
        keystates[r] = keystates[r] | (64 >> c);
      }
      else
      {
        keystates[r] = keystates[r] & ~(64 >> c);
      }
    }
    digitalWrite(rows[r], HIGH);
  }
}

void requestEvent()
{
  Wire1.write(keystates, 4);
}
