import UserService from "../services/UserService";
import NoteService from "../services/NoteService";

const userService = new UserService();
const noteService = new NoteService();

export const useApi = () => ({
  validateUserField: async (field, value) => {
    return await userService.validateUserField(field, value);
  },
  validateLogin: async (logInfoSubmission) => {
    return await userService.validateLogin(logInfoSubmission);
  },
  isRegistered: async (user) => {
    return await userService.isRegistered(user);
  },
  registerUser: async (user) => {
    return await userService.registerUser(user);
  },
  getDBUsers: async () => {
    return await userService.getDBUsers();
  },
  updateUserField: async (data) => {
    return await userService.updateUserField(data);
  },
  getUserField: async (data) => {
    return await userService.getUserField(data);
  },
  getUserByField: async (data) => {
    return await userService.getUserByField(data);
  },
  getNoteId: async (data) => {
    return await noteService.getNoteId(data);
  },
  registerNote: async (note) => {
    await noteService.registerNote(note);
  },
  removeNote: async (note) => {
    await noteService.removeNote(note);
  },
  updateNote: async (note) => {
    await noteService.updateNote(note);
  },
  getNotesByCreatorId: async (userId) => {
    return await noteService.getNotesByCreatorId(userId);
  },
  getUserWarnings: async (userId) => {
    return await noteService.getUserWarnings(userId);
  }
});