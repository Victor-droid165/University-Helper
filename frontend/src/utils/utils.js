export const capitalize = (str) => {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

export const validateUser = async (api, userFields) => {
    const validationPromises = Object.keys(userFields).map(async (field) => {
        if (field === 'type') return "Success";
        return api.validateUserField(field, userFields[field]);
    });

    const validationResults = await Promise.all(validationPromises);
    const validationErrors = validationResults.reduce((errors, error, index) => {
        if (error.startsWith('Erro')) {
            errors[Object.keys(userFields)[index] + 'Error'] = error;
        }
        return errors;
    }, {});

    return validationErrors;
}