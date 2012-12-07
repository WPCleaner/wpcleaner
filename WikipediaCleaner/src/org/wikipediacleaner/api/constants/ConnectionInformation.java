/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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

package org.wikipediacleaner.api.constants;

import org.wikipediacleaner.api.data.User;


/**
 * Bean for storing connection informations.
 */
public class ConnectionInformation {

  /**
   * Parameter <code>lgToken</code> for API calls.
   */
  private String lgToken;

  /**
   * Parameter <code>lgUserName</code> for API calls. 
   */
  private String lgUserName;

  /**
   * Parameter <code>lgUserId</code> for API calls.
   */
  private String lgUserId;

  /**
   * User.
   */
  private User user;

  /**
   * Edit token.
   */
  private String editToken;

  public ConnectionInformation() {
    clean();
  }

  /**
   * Clean-up connection information.
   */
  public void clean() {
    lgToken = null;
    lgUserName = null;
    lgUserId = null;
    editToken = null;
  }

  /**
   * @return True if connection is cleaned up.
   */
  public boolean isClean() {
    return (lgToken == null) && (lgUserName == null) && (lgUserId == null);
  }

  /**
   * @param token Parameter <code>lgtoken</code> for API calls.
   * @param userName Parameter <code>lgusername</code> for API calls.
   * @param userId Parameter <code>lguserid</code> for API calls.
   */
  public void setLgInformation(String token, String userName, String userId) {
    setLgToken(token);
    setLgUserName(userName);
    setLgUserId(userId);
  }

  /**
   * @return Parameter <code>lgtoken</code> for API calls.
   */
  public String getLgToken() {
    return lgToken;
  }

  /**
   * @param token Parameter <code>lgtoken</code> for API calls.
   */
  void setLgToken(String token) {
    lgToken = token;
  }

  /**
   * @return Parameter <code>lgusername</code> for API calls.
   */
  public String getLgUserName() {
    return lgUserName;
  }

  /**
   * @param userName Parameter <code>lgUserName</code> for API calls.
   */
  void setLgUserName(String userName) {
    lgUserName = userName;
  }

  /**
   * @return Parameter <code>lguserid</code> for API calls.
   */
  public String getLgUserId() {
    return lgUserId;
  }

  /**
   * @param userId Parameter <code>lgUserId</code> for API calls.
   */
  void setLgUserId(String userId) {
    lgUserId = userId;
  }

  /**
   * @return User.
   */
  public User getUser() {
    return user;
  }

  /**
   * @param user User.
   */
  public void setUser(User user) {
    this.user = user;
  }

  /**
   * @return Edit token.
   */
  public String getEditToken() {
    return editToken;
  }

  /**
   * @param token Edit token.
   */
  public void setEditToken(String token) {
    this.editToken = token;
  }
}
