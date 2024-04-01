const apiNotesURL = process.env.REACT_APP_API + "/notes"
const getNoteIdRoute = apiNotesURL + "/getId"
const removeNoteRoute = apiNotesURL + "/removeNote"
const registerNoteRoute = apiNotesURL + "/registerNote"
const updateNoteRoute = apiNotesURL + "/updateANote"
const notifyUserRoute = apiNotesURL + "/notifyUser"
const userNotificationsRoute = apiNotesURL + "/userNotifications"

export default class NoteService {

  async getNoteId(prefix) {
    const response = await fetch(getNoteIdRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(prefix),
    });
    const data = await response.json();
    return data;
  }

  async removeNote(note) {
    await fetch(removeNoteRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(note),
    })
  }

  async registerNote({ warnedUser, ...note }) {
    await fetch(registerNoteRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(note),
    });
    console.log(warnedUser, note);
    if (note.noteType === 'Warning') {
      await this.warnUser({ dbWarningId: note.noteId, dbWarnedUserId: warnedUser.dbUserId });
    }
  }

  async warnUser(warningNotification) {
    await fetch(notifyUserRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(warningNotification),
    });
  }

  async updateNote({ warnedUser, ...note }) {
    await fetch(updateNoteRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(note),
    });
    if (note.noteType === 'Warning') {
      await this.warnUser({ dbWarningId: note.noteId, dbWarnedUserId: warnedUser.dbUserId });
    }
  }

  async getNotesByCreatorId(userId) {
    const response = await fetch('http://localhost:8081/api/notes/notes', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(userId.toString()),
    });
    return await response.json();
  }

  async getUserWarnings(userId) {
    const queryParams = new URLSearchParams({ dbUserId: userId }).toString();
    const url = userNotificationsRoute + `?${queryParams}`;
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      }
    });
    return await response.json();
  }

}