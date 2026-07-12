/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.magicword;


/**
 * Definition of an unknown magic word type.
 */
class UnknownMagicWordType extends MagicWordType {

  public UnknownMagicWordType(String name) {
    super(name, false, false, false, false);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    UnknownMagicWordType unknownMagicWordType = (UnknownMagicWordType) obj;
    return name.equals(unknownMagicWordType.name);
  }
}
