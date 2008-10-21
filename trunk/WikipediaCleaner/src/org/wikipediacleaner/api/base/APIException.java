/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.api.base;


/**
 * Generic API Exception.
 */
public class APIException extends Exception {

  /**
   * 
   */
  private static final long serialVersionUID = 6413874942788957653L;

  /**
   * Constructor.
   */
  public APIException() {
    super();
  }

  /**
   * @param message Exception message.
   */
  public APIException(String message) {
    super(message);
  }

  /**
   * @param cause Exception cause.
   */
  public APIException(Throwable cause) {
    super(cause);
  }

  /**
   * @param message Exception message.
   * @param cause Exception cause.
   */
  public APIException(String message, Throwable cause) {
    super(message, cause);
  }

}
