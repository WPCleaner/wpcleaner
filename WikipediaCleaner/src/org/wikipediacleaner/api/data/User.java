/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;


/**
 * Bean for holding user information.
 */
public class User {

  public final static String RIGHT_DELETE = "delete";
  public final static String RIGHT_EDIT = "edit";
  public final static String RIGHT_MOVE = "move";

  /**
   * User name.
   */
  private final String name;

  /**
   * List of groups.
   */
  private List<String> groups;

  /**
   * List of rights.
   */
  private List<String> rights;

  /**
   * @param name User name.
   */
  public User(String name) {
    this.name = name;
  }

  /**
   * @return User name.
   */
  public String getName() {
    return name;
  }

  /**
   * @param groups Groups.
   */
  public void setGroups(Collection<String> groups) {
    if (groups == null) {
      this.groups = null;
    } else {
      this.groups = new ArrayList<String>(groups);
    }
  }

  /**
   * @param group Group.
   * @return True if the user is a member of the group.
   */
  public boolean isMemberOf(String group) {
    if ((group == null) || (groups == null)) {
      return false;
    }
    return groups.contains(group);
  }

  /**
   * @param rights Rights.
   */
  public void setRights(Collection<String> rights) {
    if (rights == null) {
      this.rights = null;
    } else {
      this.rights = new ArrayList<String>(rights);
    }
  }

  /**
   * @param right Right.
   * @return True if the user has the right.
   */
  public boolean hasRight(String right) {
    if ((right == null) || (rights == null)) {
      return false;
    }
    return rights.contains(right);
  }
}
