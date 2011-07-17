/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.utils;

import java.awt.Color;


/**
 * A simple POJO to hold informations about styles.
 */
public class ConfigurationStyle {

  private boolean enabled;

  private boolean foreground;
  private Color foregroundValue;
  private boolean background;
  private Color backgroundValue;

  private boolean italic;
  private boolean bold;
  private boolean underline;
  private boolean strikeThrough;

  public ConfigurationStyle() {
    this.enabled = true;
    this.foreground = false;
    this.foregroundValue = Color.BLACK;
    this.background = false;
    this.backgroundValue = Color.WHITE;
    this.italic = false;
    this.bold = false;
    this.underline = false;
    this.strikeThrough = false;
  }

  public boolean getEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public boolean getForeground() {
    return foreground;
  }

  public void setForeground(boolean foreground) {
    this.foreground = foreground;
  }

  public Color getForegroundValue() {
    return foregroundValue;
  }

  public void setForegroundValue(Color color) {
    this.foregroundValue = color;
  }

  public boolean getBackground() {
    return background;
  }

  public void setBackground(boolean background) {
    this.background = background;
  }

  public Color getBackgroundValue() {
    return backgroundValue;
  }

  public void setBackgroundValue(Color color) {
    this.backgroundValue = color;
  }

  public boolean getItalic() {
    return italic;
  }

  public void setItalic(boolean italic) {
    this.italic = italic;
  }

  public boolean getBold() {
    return bold;
  }

  public void setBold(boolean bold) {
    this.bold = bold;
  }

  public boolean getUnderline() {
    return underline;
  }

  public void setUnderline(boolean underline) {
    this.underline = underline;
  }

  public boolean getStrikeThrough() {
    return strikeThrough;
  }

  public void setStrikeThrough(boolean strikeThrough) {
    this.strikeThrough = strikeThrough;
  }
}
